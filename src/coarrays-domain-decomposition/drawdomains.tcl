# drawdomains.tcl --
#     Draw some simple diagrams
#
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

pack [canvas .c -width 650 -height 450]

for {set x 0} {$x < 21} {incr x} {
    set px [expr {$x * 10}]

    .c create line $px   0 $px 200 -tag A -fill lightgrey
}

set px 200
foreach tag {A B C} {

    for {set y 0} {$y < 21} {incr y} {
        set py [expr {$y * 10}]

        .c create line   0 $py $px $py -tag $tag -fill lightgrey
    }

    set px 400
}

foreach tag  {B C} {
    for {set x 0} {$x < 41} {incr x} {
        set px [expr {$x * 10}]

        .c create line $px   0 $px 200 -tag $tag -fill lightgrey
    }
}

.c create rectangle   0   0  200  200 -tag A -width 2
.c create rectangle   0   0  400  200 -tag B -width 2
.c create rectangle   0   0  400  200 -tag C -width 2

.c create text       20  20  -text "Domain 1:\n20 rows, 20 columns" -tag A -anchor nw
.c create text       20  20  -text "Domain 2:\n40 rows, 20 columns" -tag B -anchor nw
.c create text       20  20  -text "Domain 3:\n40 rows, 20 columns" -tag C -anchor nw

.c move A  20 220
.c move B 220 220
.c move C 220  20

.c create text   10 320 -text "Open: C = 1" -angle 90
.c create text  110 440 -text "Closed"
.c create text  110 205 -text "Closed"
.c create text  430 440 -text "Open: C = 0"
.c create text  630 320 -text "Closed" -angle 90
.c create text  630 120 -text "Closed" -angle 90
.c create text  210 120 -text "Closed" -angle 90
.c create text  430  10 -text "Closed"

after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {6.7c 4.6c}]
    $pdf1 canvas .c -width 6.5c
    $pdf1 write -file diagram_domains.pdf
}
