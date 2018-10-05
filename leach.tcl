
source /home/anurag/Desktop/NEW/ns-allinone-2.34/ns-2.34/mit/uAMPS/ns-leach.tcl
set opt(num_clusters) 5
set opt(filename) alg
set opt(dirname) "/home/anurag/Desktop/NEW/ns-allinone-2.34/ns-2.34/mit/leach_sims"
set opt(init_energy) 2
set opt(eq_energy) 1
set opt(bs_x) 175
set opt(bs_y) 175
set opt(max_dist) 500
set opt(rcapp)        "Application/LEACH"        ;# Application type
set opt(tr)           "/tarik/leach.tr"            ;# Trace file
set opt(spreading)    [expr int([expr 1.5*$opt(num_clusters)])+1]

set outf [open "$opt(dirname)/conditions.txt" w]
puts $outf "\nUSING LEACH: DISTRIBUTED CLUSTER FORMATION\n"
close $outf

source /home/anurag/Desktop/NEW/ns-allinone-2.34/ns-2.34/mit/uAMPS/sims/uamps.tcl


# Parameters for distrbuted cluster formation algorithm
                                          ;# RA Time (s) for CH ADVs
set opt(ra_adv)       [TxTime [expr $opt(hdr_size) + 4]]  
                                          ;# Total time (s) for CH ADVs
                                          ;# Assume max 4(nn*%) CHs
set opt(ra_adv_total) [expr $opt(ra_adv)*($opt(num_clusters)*4 + 1)]
                                          ;# RA Time (s) for nodes' join reqs
set opt(ra_join)      [expr 0.01 * $opt(nn_)]             
                                          ;# Buffer time for join req xmittal
set opt(ra_delay)     [TxTime [expr $opt(hdr_size) + 4]]         
                                          ;# Maximum time required to transmit 
                                          ;# a schedule (n nodes in 1 cluster)
set opt(xmit_sch)     [expr 0.005 + [TxTime [expr $opt(nn_)*4+$opt(hdr_size)]]]
                                          ;# Overhead time for cluster set-up
set opt(start_xmit)   [expr $opt(ra_adv_total) + $opt(ra_join) + $opt(xmit_sch)]


set outf [open "$opt(dirname)/conditions.txt" a]
if {$opt(eq_energy) == 1} {
  puts $outf "Thresholds chosen using original probs."
} else {
  puts $outf "Thresholds chosen using energy probs."
}
puts $outf "Desired number of clusters = $opt(num_clusters)"
puts $outf "Spreading factor = $opt(spreading)"
puts $outf "Changing clusters every $opt(ch_change) seconds\n"
close $outf

