{application,inotify,
             [{description,"Linux file alternation monitor."},
              {vsn,"0.4.0"},
              {registered,[inotify_server,inotify_evt]},
              {applications,[kernel,stdlib]},
              {mod,{inotify,[]}},
              {modules,[inotify,inotify_evt,inotify_server]}]}.
