-module(config).
-compile(export_all).

log_level()   -> info.
log_modules() -> [login,index,n2o_proto,n2o_nitrogen,kvs,store_aerospike].
