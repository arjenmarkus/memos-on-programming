! corner.f90 --
!     Solve a diffusion problem defined on several connected domains
!
!     Note:
!     Stopping the images that have no role in the calculation means
!     that a "sync all" statement causes a deadlock. It will wait for
!     all original images.
!
!     Hm, keeping a list of running images does not solve the problem
!     At least not with this version of Ifort (2021.9.0)
!
!     Use teams to split off the inactive images
!
program corner
    use iso_fortran_env

    implicit none

    type(team_type) :: active_inactive

    type domain_type
        integer               :: nx, ny
        integer, dimension(4) :: boundary_type          = -1
        integer, dimension(4) :: connecting_domain      = -1
        real, dimension(4)    :: boundary_value         = -999.0
        real                  :: initial_concentration  = -999.0
    end type domain_type

    type(domain_type) :: domain

    real, allocatable :: left_concentration(:)[:]
    real, allocatable :: right_concentration(:)[:]
    real, allocatable :: top_concentration(:)[:]
    real, allocatable :: bottom_concentration(:)[:]
    real, allocatable :: conc(:,:)
    real, allocatable :: dconc(:,:)

    integer, allocatable :: running_image[:]
    integer, allocatable :: list_running_images(:)

    character(len=80) :: filename
    integer           :: i, ierr
    integer           :: xmax, ymax, length, width
    integer           :: this_team

    integer           :: timespan
    real              :: diff_factor

    allocate( running_image[*] )
    running_image = 0

    write( filename, '(a,i0,a)' ) 'corner_', this_image(), '.inp'

    open( 10, file = filename, status = 'old', iostat = ierr )

    if ( ierr == 0 ) then
        running_image = this_image()
        call read_input( domain )

        write(*,*) 'Image ', this_image(), domain, ' --> ', running_image
        this_team = 1
    else
        this_team = 2
        write(*,*) 'Image ', this_image(), ' inactive'
    endif

    !
    ! We need to make sure all images have finished the initialisation
    !
    form team ( this_team, active_inactive )

    !
    ! Select the team and enter the team construct
    !
    change team ( active_inactive )

        if ( team_number() == 1 ) then

            !
            ! Prepare the calculation
            !
            xmax = domain%nx
            ymax = domain%ny
            call co_max( xmax )
            call co_max( ymax )

            call co_broadcast( timespan, 1 )
            call co_broadcast( diff_factor, 1 )

            if ( this_image() == 1 ) then
                write(*,*) 'Overall grid sizes: ', xmax, ymax
            endif

            allocate( left_concentration(ymax+2)[*]   )
            allocate( right_concentration(ymax+2)[*]  )
            allocate( top_concentration(xmax+2)[*]    )
            allocate( bottom_concentration(xmax+2)[*] )

            allocate( conc(domain%nx+2,domain%ny+2)  )
            allocate( dconc(domain%nx+2,domain%ny+2) )

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
                    write(*,*) this_image(), i
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
                ! Wait for all the images before copying the boundaries
                ! (note: sync all works on the current team, which is not the initial team at this point)
                !
                sync all

                ! Left
                if ( domain%boundary_type(1) == 2 ) then
                    conc(1,:) = right_concentration(1:width+2)[domain%connecting_domain(1)]
                elseif ( domain%boundary_type(1) == 0 ) then
                    conc(1,:) = conc(2,:)
                endif
                ! Right
                if ( domain%boundary_type(2) == 2 ) then
                    conc(length+2,:) = left_concentration(1:width+2)[domain%connecting_domain(2)]
                elseif ( domain%boundary_type(2) == 0 ) then
                    conc(length+2,:) = conc(length+1,:)
                endif
                ! Top
                if ( domain%boundary_type(3) == 2 ) then
                    conc(:,width+2) = bottom_concentration(1:length+2)[domain%connecting_domain(3)]
                elseif ( domain%boundary_type(3) == 0 ) then
                    conc(:,width+2) = conc(:,width+1)
                endif
                ! Bottom
                if ( domain%boundary_type(4) == 2 ) then
                    conc(:,1) = top_concentration(1:length+2)[domain%connecting_domain(4)]
                elseif ( domain%boundary_type(4) == 0 ) then
                    conc(:,1) = conc(:,2)
                endif

                !
                ! And when everything has been copied, continue
                !
                sync all
            enddo


            write(*,*) 'Image ', this_image(), ' has reached the end'


            write( filename, '(a,i0,a)' ) 'report_corner_', this_image(), '.out'

            open( 20, file = filename )
            do i = 1,width
                write( 20, '(*(g10.3))' ) conc(:,i)
            enddo
        else
            !
            ! The second team has no task ...
            !
            write(*,*) 'Image ', this_image(), ' is idle'
        endif
    end team

    !
    ! Wait for all images
    !
    sync all

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
