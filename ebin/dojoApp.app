{application,dojoApp,
             [{description,"dojo Application"},
              {vsn,"0.1"},
              {registered,[dojoNetwork,dojoManager]},
              {applications,[kernel,stdlib]},
              {mod,{dojoApp_app,[]}},
              {env,[]},
              {modules,[dojoApp_app,dojoApp_sup,dojoDB,dojoIO,dojoManager,
                        dojoNetwork,dojoNode,json_eep,mnesia_test]}]}.