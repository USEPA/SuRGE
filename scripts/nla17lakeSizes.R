# NLA17 data on lake size and area distribution.
# Data provide by Tony Olsen, 10/16/2019

area <- read_excel("inputData/nla2017/nla17lakeNumberAndArea.xlsx", 
                   sheet = "area") %>%
  melt(id.vars = "ER") %>%
  mutate(value = trim(value), 
         value = as.numeric(value),
         lake.size = as.character(variable) %>% trim()) %>%
  rename(area.ha = value) %>%
  filter(lake.size != "Sum") %>%
  mutate(lake.size = factor(lake.size, 
                            # levels are based in Ha
                            levels = c("(1,4]", "(4,10]", "(10,20]", "(20,50]", ">50"),
                            # relabeling as km2 for convenience
                            labels = c("(0.01, 0.04]", "(0.04, 0.1]", "(0.1, 0.2]", "(0.2, 0.5]", ">0.5")))

ggplot(area, aes(lake.size, area.ha)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ER, scales = "free_y") +
  xlab("lake size bins (km2)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Lake size distributions in NLA17-sampled")

ggsave("output/figures/nla17lakeSizeDistribution.tiff")
