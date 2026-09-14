export ERL_TOP=/home/intjelic/Workspace/erlang/build/otp_src_26.2.5.6
$ERL_TOP/bin/cerl -debug -pa test -pa ebin -eval 'test_event_handlers:run(), erlang:halt()'
