# drawsubgrid.tcl --
#     Draw some simple diagrams
#
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

pack [canvas .c -width 300 -height 220]

.c create rectangle  30  30 270 180 -fill lightgrey
.c create rectangle  70  60 230 150 -fill white

for {set x 0} {$x < 7} {incr x} {
    set px [expr {30 + $x * 40}]

    .c create line $px  30 $px 180
}

for {set y 0} {$y < 6} {incr y} {
    set py [expr {30 + $y * 30}]

    .c create line  30 $py 270 $py
}

.c create text  20 105 -text "Left"   -anchor s -angle 90
.c create text 280 105 -text "Right"  -anchor n -angle 90
.c create text 150  20 -text "Top"    -anchor s
.c create text 150 190 -text "Bottom" -anchor n

after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {6.5c 4.6c}]
    $pdf1 canvas .c -width 6.2c
    $pdf1 write -file diagram_subgrid.pdf
}
