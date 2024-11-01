! corner_mpi.f90 --
!     Solve a diffusion problem defined on several connected domains
!
!     Note:
!     This version uses MPI to do the parallellisation.
!
!     Note:
!     The indexing of the processes starts at 0 instead of 1 and that
!     causes confusion. But also the fact that a connection from one
!     domain to the adjacent one changes the edge index is confusing.
!
!     I desperately need to use symbols (parameters) instead of
!     integers.
!
program corner
    use mpi
    use iso_fortran_env

    implicit none

    type domain_type
        integer               :: nx, ny
        integer, dimension(4) :: boundary_type          = -1
        integer, dimension(4) :: connecting_domain      = -1
        integer, dimension(4) :: destination            = -1
        real, dimension(4)    :: boundary_value         = -999.0
        real                  :: initial_concentration  = -999.0
    end type domain_type

    type(domain_type) :: domain

    real, allocatable :: left_concentration(:)
    real, allocatable :: right_concentration(:)
    real, allocatable :: top_concentration(:)
    real, allocatable :: bottom_concentration(:)
    real, allocatable :: conc(:,:)
    real, allocatable :: dconc(:,:)

    character(len=80) :: filename
    integer           :: i, ierr, j
    integer           :: xmax, ymax, length, width
    integer           :: runnin

    integer           :: timespan
    real              :: diff_factor

    integer, dimension(:,:), allocatable :: connection
    integer           :: process_Rank, size_Of_Cluster, ierror, tag, root
    type(MPI_Status)  :: status
    integer           :: side(4) = [2, 1, 4, 3]

    !
    ! Initialise the MPI environment
    !
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

    root = 0

    !
    ! Read the input and check the number of processes
    !
    write( filename, '(a,i0,a)' ) 'corner_', process_Rank+1, '.inp'

    open( 10, file = filename, status = 'old', iostat = ierr )

    if ( ierr == 0 ) then
        call read_input( domain )
    else
        !
        ! Simple error handling
        !
        write(*,*) 'Process ', process_Rank+1, ' has no input - correct number of processes'
        error stop
    endif

    !
    ! Prepare the calculation
    !
    call MPI_ALLREDUCE( domain%nx, xmax, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror )
    call MPI_ALLREDUCE( domain%ny, ymax, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror )

    call MPI_BCAST( timespan,    1, MPI_INTEGER, root, MPI_COMM_WORLD, ierror )
    call MPI_BCAST( diff_factor, 1, MPI_REAL,    root, MPI_COMM_WORLD, ierror )

    if ( process_Rank == 0 ) then
        write(*,*) 'Overall grid sizes: ', xmax, ymax
    endif

    allocate( left_concentration(ymax+2)   )
    allocate( right_concentration(ymax+2)  )
    allocate( top_concentration(xmax+2)    )
    allocate( bottom_concentration(xmax+2) )

    allocate( conc(domain%nx+2,domain%ny+2)  )
    allocate( dconc(domain%nx+2,domain%ny+2) )

    !
    ! Exchange the connectivity information:
    ! Gather the information on the root process and then send it off to each
    ! process. In turn, each process examines the array connection to fill
    ! the destination field.
    !
    allocate( connection(4,size_Of_Cluster) )

    write(*,*) 'Connecting: ', process_Rank, ' -- ', domain%connecting_domain

    call MPI_GATHER( domain%connecting_domain, size(domain%connecting_domain), MPI_INTEGER, &
                     connection, size(connection,1), MPI_INTEGER, root, MPI_COMM_WORLD, ierror )


    call MPI_Barrier( MPI_COMM_WORLD, ierror )

    call MPI_BCAST( connection, size(connection), MPI_INTEGER, root, MPI_COMM_WORLD, ierror )

    call MPI_Barrier( MPI_COMM_WORLD, ierror )

    do j = 1,size(connection,2)
        do i = 1,size(connection,1)
            if ( connection(i,j) == process_Rank+1 ) then
                domain%destination(side(i)) = j - 1      ! Shift by 1 required
            endif
        enddo
    enddo

    write(*,*) 'Destination:', process_Rank, ' -- ', domain%destination

    !
    ! Set up the concentrations
    !
    length = domain%nx
    width  = domain%ny

    if ( domain%initial_concentration /= -999.0 ) then
        conc = domain%initial_concentration
    else
        conc = 0.0
    endif

    ! Left
    if ( domain%boundary_type(1) == 1 ) then
        conc(1,:) = domain%boundary_value(1)
    endif
    ! Right
    if ( domain%boundary_type(2) == 1 ) then
        conc(length+2,:) = domain%boundary_value(2)
    endif
    ! Top
    if ( domain%boundary_type(3) == 1 ) then
        conc(:,width+2) = domain%boundary_value(3)
    endif
    ! Bottom
    if ( domain%boundary_type(4) == 1 ) then
        conc(:,1) = domain%boundary_value(4)
    endif

    !
    ! Do the calculation
    !
    do i = 1,timespan
        if ( mod(i,100) == 0 ) then
            write(*,*) process_Rank, i
        endif

        dconc = 0.0
        dconc(2:length+1,2:width+1) = diff_factor * ( conc(1:length,2:width+1) + conc(3:length+2,2:width+1) &
                                                      + conc(2:length+1,1:width) + conc(2:length+1,3:width+2) &
                                                      - 4.0 * conc(2:length+1,2:width+1) )
        ! Or use an explicit loop ...
        !
        !do k = 2,length+1
        !    do l = 2,width+1
        !        dconc(k,l) = diffusion * ( conc(k-1,l) + conc(k+1,l) + conc(k,l-1) + conc(k,l+1) - 4.0 * conc(k,l) )
        !    enddo
        !enddo

        conc  = conc + dconc

        left_concentration(1:width+2)    = conc(2,:)
        right_concentration(1:width+2)   = conc(length+1,:)
        top_concentration(1:length+2)    = conc(:,width+1)
        bottom_concentration(1:length+2) = conc(:,2)

        !
        ! We can send these arrays without having to wait. Use the tag to identify
        ! the boundary.
        ! For now: use blocking send and receive
        !
        if ( domain%destination(1) /= -1 ) then
            call MPI_SEND( left_concentration,   width+2,  MPI_REAL, domain%destination(1), 2, MPI_COMM_WORLD, ierror )
            write(*,*) 'Sent - left: ', process_Rank
        endif
        if ( domain%destination(2) /= -1 ) then
            call MPI_SEND( right_concentration,  width+2,  MPI_REAL, domain%destination(2), 1, MPI_COMM_WORLD, ierror )
            write(*,*) 'Sent - right: ', process_Rank
        endif
        if ( domain%destination(3) /= -1 ) then
            call MPI_SEND( top_concentration,    length+2, MPI_REAL, domain%destination(3), 4, MPI_COMM_WORLD, ierror )
            write(*,*) 'Sent - top: ', process_Rank
        endif
        if ( domain%destination(4) /= -1 ) then
            call MPI_SEND( bottom_concentration, length+2, MPI_REAL, domain%destination(4), 3, MPI_COMM_WORLD, ierror )
            write(*,*) 'Sent - bottom: ', process_Rank
        endif

        !
        ! Wait for all the processes before copying the boundaries
        ! We need to know which domains want our information!
        !
        write(*,*) 'Reaching barrier ...', process_Rank
        call MPI_Barrier( MPI_COMM_WORLD, ierror )
        write(*,*) 'Proceeding after barrier ...', process_Rank

        ! Left
        if ( domain%boundary_type(1) == 2 ) then
            write(*,*) 'Receiving left - ', process_Rank, ' -- right'
            call MPI_RECV( right_concentration, width+2, MPI_REAL, domain%connecting_domain(1)-1, 1, MPI_COMM_WORLD, status, ierror )
            conc(1,:) = right_concentration(1:width+2)
            write(*,*) 'Received left - ', process_Rank, ' -- right'
        elseif ( domain%boundary_type(1) == 0 ) then
            conc(1,:) = conc(2,:)
        endif
        ! Right
        if ( domain%boundary_type(2) == 2 ) then
            write(*,*) 'Receiving right - ', process_Rank, ' -- left'
            call MPI_RECV( left_concentration, width+2, MPI_REAL, domain%connecting_domain(2)-1, 2, MPI_COMM_WORLD, status, ierror )
            conc(length+2,:) = left_concentration(1:width+2)
            write(*,*) 'Received right - ', process_Rank, ' -- left'
        elseif ( domain%boundary_type(2) == 0 ) then
            conc(length+2,:) = conc(length+1,:)
        endif
        ! Top
        if ( domain%boundary_type(3) == 2 ) then
            write(*,*) 'Receiving top - ', process_Rank, ' -- bottom'
            call MPI_RECV( bottom_concentration, length+2, MPI_REAL, domain%connecting_domain(3)-1, 3, MPI_COMM_WORLD, status, ierror )
            conc(:,width+2) = bottom_concentration(1:length+2)
            write(*,*) 'Received top - ', process_Rank, ' -- bottom'
        elseif ( domain%boundary_type(3) == 0 ) then
            conc(:,width+2) = conc(:,width+1)
        endif
        ! Bottom
        if ( domain%boundary_type(4) == 2 ) then
            write(*,*) 'Receiving bottom - ', process_Rank, ' -- top'
            call MPI_RECV( top_concentration, length+2, MPI_REAL, domain%connecting_domain(4)-1, 4, MPI_COMM_WORLD, status, ierror )
            conc(:,1) = top_concentration(1:length+2)
            write(*,*) 'Received bottom - ', process_Rank, ' -- top'
        elseif ( domain%boundary_type(4) == 0 ) then
            conc(:,1) = conc(:,2)
        endif

        !
        ! And when everything has been copied, continue
        !
        !!write(*,*) 'Enddo barrier ', process_Rank
        call MPI_Barrier( MPI_COMM_WORLD, ierror )
    enddo


    write(*,*) 'Process ', process_Rank, ' has reached the end'


    write( filename, '(a,i0,a)' ) 'report_mpi_corner_', process_Rank+1, '.out'

    open( 20, file = filename )
    do i = 1,width
        write( 20, '(*(g10.3))' ) conc(:,i)
    enddo

    !
    ! Wait for all images
    !
    call MPI_Barrier( MPI_COMM_WORLD, ierror )
    call MPI_FINALIZE(ierror)

contains

subroutine read_input( domain )
    type(domain_type), intent(inout) :: domain

    character(len=80) :: line
    character(len=20) :: keyword, type
    integer           :: ierr, k

    do
         read( 10, '(a)', iostat = ierr ) line

         if ( ierr /= 0 ) then
             exit
         endif

         read( line, *, iostat = ierr ) keyword

         select case ( keyword )
             case( '#' )
                 ! Simply skip

             case( 'timespan' )
                 read( line, *, iostat = ierr ) keyword, timespan

             case( 'diff-factor' )
                 read( line, *, iostat = ierr ) keyword, diff_factor

             case( 'grid' )
                 read( line, *, iostat = ierr ) keyword, domain%nx, domain%ny

             case( 'initial' )
                 read( line, *, iostat = ierr ) keyword, domain%initial_concentration

             case( 'left-boundary', 'right-boundary', 'top-boundary', 'bottom-boundary' )
                 select case ( keyword )
                     case( 'left-boundary' )
                         k = 1
                     case( 'right-boundary' )
                         k = 2
                     case( 'top-boundary' )
                         k = 3
                     case( 'bottom-boundary' )
                         k = 4
                 end select

                 read( line, *, iostat = ierr ) keyword, type

                 select case ( type )
                     case( 'closed' )
                         domain%boundary_type(k) = 0
                         read( line, *, iostat = ierr ) keyword, type

                     case( 'open' )
                         domain%boundary_type(k) = 1
                         read( line, *, iostat = ierr ) keyword, type, domain%boundary_value(k)

                     case( 'image' )
                         domain%boundary_type(k) = 2
                         read( line, *, iostat = ierr ) keyword, type, domain%connecting_domain(k)
                 end select
         end select
     enddo
end subroutine read_input
end program corner
