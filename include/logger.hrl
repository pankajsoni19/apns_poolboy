-ifndef(PRINT).

-define(PRINT(Format, Args), 
    _ = io:format(Format, Args)).

-define(DEBUG(Format, Args), 
    _ = lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args), 
    _ = lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args), 
    _ = lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args), 
    _ = lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args), 
    _ = lager:critical(Format, Args)).

-endif.