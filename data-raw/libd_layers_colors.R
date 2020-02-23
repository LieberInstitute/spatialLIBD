## Colors from Lukas M Weber adjusted for the purple background
libd_layer_colors <- c("#F0027F", "#377EB8", "#4DAF4A", "#984EA3", "#FFD700", "#FF7F00", "#1A1A1A", 'transparent', "#666666")
names(libd_layer_colors) <- c(paste0('Layer', seq_len(6)), 'WM', 'NA', 'WM2')

use_data(libd_layer_colors)
