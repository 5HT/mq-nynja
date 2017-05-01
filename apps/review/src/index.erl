-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").

event(init) ->
    Room = code(),
    io:format("Index INIT: ~p~n",[{Room,n2o:user()}]),
    nitro:update(logout,  #button { id=logout,  body="Logout "  ++ n2o:user(),       postback=logout }),
    nitro:update(send,    #button { id=send,    body="Chat",       source=[message], postback=chat   }),
    nitro:update(heading, #b      { id=heading, body="Review: " ++ Room}),
    nitro:update(upload,  #upload { id=upload   }),
    [ event(#client{data={E#entry.from,E#entry.media}})
      || E <- kvs:entries(kvs:get(feed,{room,Room}),entry,10) ];

event(chat) ->
    User = n2o:user(),
    Message = n2o:q(message),
    Room = code(),
    io:format("Chat pressed: ~p ~p~n",[Room,self()]),
    kvs:add(#entry{id=kvs:next_id("entry",1),
                   from=n2o:user(),feed_id={room,Room},media=Message}),
    Msg = emqttd_message:make(Room, 0, Room, term_to_binary(#client{data={User,Message}})),
    self() ! {deliver, Msg};

event(#client{data={User,Message}}) ->
     nitro:wire(#jq{target=message,method=[focus,select]}),
     HTML = nitro:to_list(Message),
     DTL = #dtl{file="message",
                app=review,
                bindings=[{user,User},{color,"gray"},{message,HTML}]},
     nitro:insert_top(history, nitro:jse(nitro:render(DTL)));

event(#ftp{sid=Sid,filename=Filename,status={event,stop}}=Data) ->
    io:format("FTP Delivered ~p~n",[Data]),
    Name = hd(lists:reverse(string:tokens(wf:to_list(Filename),"/"))),
    erlang:put(message,
    nitro:render(#link{href=iolist_to_binary(["/spa/",Sid,"/",nitro_conv:url_encode(Name)]),body=Name})),
    event(chat);

event(logout) -> n2o:redirect("login.htm");
event(Event)  -> io:format("Event: ~p", [Event]).

main() -> [].
code() -> case get(topic) of undefined -> "lobby";
                                  Code -> nitro:to_list(Code) end.

