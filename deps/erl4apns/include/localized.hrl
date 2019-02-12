
-record(loc_alert, {body    = none  :: none | erl4apns:apns_str(),
                    action  = none  :: none | erl4apns:apns_str(),
                    key     = ""    :: erl4apns:apns_str(),
                    args    = []    :: [erl4apns:apns_str()],
                    image   = none  :: none | erl4apns:apns_str()}).
