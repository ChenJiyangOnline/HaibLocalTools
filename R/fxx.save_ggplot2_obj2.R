fxx.save_ggplot2_obj2res <- function(final_folder, p_filename, p=last_plot(), height, width, dpi = 600){
  if(!dir.exists( file.path( root_path, res_folder, final_folder) )){
    dir.create(file.path( root_path, res_folder, final_folder))
  }
  
  setwd(file.path( root_path, res_folder, final_folder))
  p_filename <- str_c(p_filename)
  ggsave(plot = p, filename = str_c(p_filename, ".png"), height = height, width = width, units = "in", dpi = dpi)
  ggsave(plot = p, filename = str_c(p_filename, ".pdf"), height = height, width = width)
} # 图片保存
fxx.save_ggplot2_obj2rdata <- function(final_folder, p_filename, p=last_plot(), height, width, dpi = 600){
  if(!dir.exists( file.path( root_path, rdata_folder, final_folder) )){
    dir.create(file.path( root_path, rdata_folder, final_folder))
  }
  
  setwd(file.path( root_path, rdata_folder, final_folder))
  p_filename <- str_c(p_filename)
  ggsave(plot = p, filename = str_c(p_filename, ".png"), height = height, width = width, units = "in", dpi = dpi)
  ggsave(plot = p, filename = str_c(p_filename, ".pdf"), height = height, width = width)
} # 图片保存
#-------- DIY  END ---------##