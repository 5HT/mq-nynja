-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> [].
code() -> case get(topic) of undefined -> "lobby";
                                  Code -> wf:to_list(Code) end.
event(init) ->
    Room = code(),
    wf:update(logout,  #button { id=logout,  body="Logout "  ++ wf:user(),        postback=logout }),
    wf:update(send,    #button { id=send,    body="Chat",       source=[message], postback=chat   }),
    wf:update(heading, #b      { id=heading, body="Review: " ++ Room}),
    wf:update(upload,  #upload { id=upload   }),
    [ event({client,{E#entry.from,E#entry.media}})
      || E <- kvs:entries(kvs:get(feed,{room,Room}),entry,10) ];

event(logout) ->
    wf:logout(),
    wf:redirect("login.htm");

event(chat) ->
    User = wf:user(),
    Message = wf:q(message),
    Room = code(),
    wf:info(?MODULE,"Chat pressed: ~p ~p~n",[Room,self()]),
    kvs:add(#entry{id=kvs:next_id("entry",1),from=wf:user(),feed_id={room,Room},media=Message}),
    Msg = emqttd_message:make(Room, 0, Room, term_to_binary(#client{data={User,Message}})),
    self() ! {deliver, Msg};

event(#client{data={User,Message}}) ->
     wf:wire(#jq{target=message,method=[focus,select]}),
     HTML = wf:to_list(Message),
     DTL = #dtl{file="message",app=review,bindings=[{user,User},{color,"gray"},{message,HTML}]},
     wf:insert_top(history, wf:jse(wf:render(DTL)));

event(#ftp{sid=Sid,filename=Filename,status={event,stop}}=Data) ->
    wf:info(?MODULE,"FTP Delivered ~p~n",[Data]),
    Name = hd(lists:reverse(string:tokens(wf:to_list(Filename),"/"))),
    erlang:put(message,wf:render(#link{href=iolist_to_binary(["/static/",Sid,"/",wf:url_encode(Name)]),body=Name})),
    wf:info(?MODULE,"Message ~p~n",[wf:q(message)]),
    event(chat);

event(Event) ->
    wf:info(?MODULE,"Event: ~p", [Event]),
    ok.
