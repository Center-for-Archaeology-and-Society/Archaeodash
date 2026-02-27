# fn = "C:\\Users\\rjbischo\\Dropbox (ASU)\\Projects\\SaffordBasinObsidianStudy\\ObsidianData.xlsx"
# data = import(fn) %>%
#   janitor::clean_names() %>%
#   filter(project_name_title %in% c("ASU CAS Obsidian Sourcing","Shackley Obsidian Sourcing"))
#
# counts = data %>%
#   group_by(place_name) %>%
#   count() %>%
#   filter(n >= 15)
# data = data %>%
#   filter(place_name %in% counts$place_name) %>%
#   select(place_name, nb:zr) %>%
#   mutate_at(vars(-place_name),as.numeric) %>%
#   mutate_at(vars(-place_name),replace_na,0)
#
# model = naive_bayes(place_name ~ ., data = data)
# pred = predict(model,data, type = "prob")
# pred = pred %>% as.data.frame() %>%  bind_cols(data %>% select(place_name)) %>%
#   mutate_at(vars(-place_name),round,2) %>%
#   rowid_to_column() %>%
#   pivot_longer(-c(place_name,rowid)) %>%
#   filter(value > 0) %>%
#   mutate(correct = ifelse(name == place_name,1,0)) %>%
#   group_by(place_name) %>%
#   slice_max(value,n = 1)
# sum(pred$correct)/nrow(pred)
# library(randomForest)
# data = data %>%
#   mutate(place_name = factor(place_name))
# model = randomForest(place_name ~ ., data = data)
# pred = predict(model,data, type = "prob")
# pred = pred %>% as.data.frame() %>%  bind_cols(data %>% select(place_name)) %>%
#   mutate_at(vars(-place_name),round,2) %>%
#   rowid_to_column() %>%
#   pivot_longer(-c(place_name,rowid)) %>%
#   filter(value > 0) %>%
#   mutate(correct = ifelse(name == place_name,1,0)) %>%
#   group_by(place_name) %>%
#   slice_max(value,n = 1)
# sum(pred$correct)/nrow(pred)
#
# artifacts = import(fn) %>%
#   janitor::clean_names() %>%
#   filter(project_name_title %in% c("Eastern Arizona Collection")) %>%
#   select(place_name, nb:zr) %>%
#   mutate_at(vars(-place_name),as.numeric)
# pred = predict(model,artifacts, type = "prob")
# pred = pred %>% as.data.frame() %>%
#   bind_cols(artifacts %>% select(place_name)) %>%
#   mutate_at(vars(-place_name),round,2) %>%
#   rowid_to_column() %>%
#   pivot_longer(-c(place_name,rowid)) %>%
#   filter(value > 0) %>%
#   mutate(correct = ifelse(name == place_name,1,0)) %>%
#   group_by(rowid) %>%
#   slice_max(value,n = 1)
