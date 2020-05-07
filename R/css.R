# if we every add another type of event to the events table,
# expand this selection to cover all our bases
my_cols <- brewer.pal(7,"Pastel2")

css <- paste0("
  .nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
  }
  
  .vis-item.DS { background-color: ",my_cols[1],"; }
  .vis-item.CM { background-color: ",my_cols[2],"; }
  .vis-item.AE { background-color: ",my_cols[3],"; }
  .vis-item.LB { background-color: ",my_cols[4],"; }
  .vis-item.MH_MH { background-color: ",my_cols[5],"; }
  .vis-item.MH_FDH { background-color: ",my_cols[6],"; }
  .vis-item.MH_DH { background-color: ",my_cols[7],"; }
")
# feel free to add more css above

my_gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n +1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

my_gg_color_hue(2)[1]
