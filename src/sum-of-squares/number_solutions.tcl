# number_solutions.tcl
#     Plot the number of solutions as function of the number
#
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

package require Plotchart
pack [canvas .c -width 600 -height 600]
set p [::Plotchart::createXLogYPlot .c {0 300 50} {1 1.0e6 10}]
#et p [::Plotchart::createLogXLogYPlot .c {1 1000 10} {1 1.0e5 10}]

set data {
10                4
20                12
30                27
40                55
50                104
60                178
70                302
80                476
90                747
100               1116
110               1653
120               2374
130               3385
140               4714
150               6521
160               8849
170               11953
180               15895
190               21031
200               27482
210               35750
220               46030
230               59027
240               75005
250               94987
260               119360
270               149482
280               185960
290               230580
300               284316
}

$p dataconfig match -colour red

foreach {x y} $data {
    $p plot data  $x $y
    #$p plot match $x [expr {0.03*exp(sqrt($x))}]
    $p plot match $x [expr {0.3*exp($x**0.46)}]
}


after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {9.5c 9.5c}]
    $pdf1 canvas .c -width 9.2c
    $pdf1 write -file number_solutions.pdf
}
