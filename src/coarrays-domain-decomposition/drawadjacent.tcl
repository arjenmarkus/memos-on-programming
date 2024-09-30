# drawadjacent.tcl --
#     Draw some simple diagrams
#
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

pack [canvas .c -width 700 -height 220]

foreach tag {A B} {
.c create rectangle  30  30 270 180 -fill lightgrey -tag $tag
.c create rectangle  70  60 230 150 -fill white -tag $tag

.c create rectangle  190  30 230 180 -fill orange
.c create rectangle  330  30 370 180 -fill orange

.c create rectangle  230  30 270 180 -fill lightblue
.c create rectangle  370  30 410 180 -fill lightblue

for {set x 0} {$x < 7} {incr x} {
    set px [expr {30 + $x * 40}]

    .c create line $px  30 $px 180 -tag "$tag line"
}

for {set y 0} {$y < 6} {incr y} {
    set py [expr {30 + $y * 30}]

    .c create line  30 $py 270 $py -tag "$tag line"
}
}

.c raise line
.c move B 300   0

.c create line 210  30 280   5 350  30 -arrow last  -width 2 -smooth 1
.c create line 250 180 320 195 390 180 -arrow first -width 2 -smooth 1

after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {7.5c 2.5c}]
    $pdf1 canvas .c -width 7.2c
    $pdf1 write -file diagram_adjacent.pdf
}
