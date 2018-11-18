
:-use_module(library(http/http_open)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).


%makes request to a site
go:-
    use_module(library(http/http_open)),
   http_open('http://www.google.com/search?q=prolog', In, []),
   copy_stream_data(In, user_output),
   close(In).


%start a http server
server(Port) :-
        http_server(http_dispatch,
                    [ port(Port)
                    ]).

:- http_handler(root(.), entry_page, []).
:- http_handler(root(home), home_page, []).