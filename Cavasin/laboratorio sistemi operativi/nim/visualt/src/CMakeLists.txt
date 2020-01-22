add_library(visualt visualt.c "${VisualT_SOURCE_DIR}/include/visualt/visualt.h")
target_include_directories(visualt PUBLIC "${VisualT_SOURCE_DIR}/include")
target_compile_features(visualt PUBLIC c_std_99)