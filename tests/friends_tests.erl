-module(friends_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_strand_test_() ->
	[{"",
	 {setup, fun init/0, fun stop/1, fun make_friends/1}}
	].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init() ->
	friends:init(),
	% friends to check
	Aaron = {"Aaron", ["Bella"]},
	Bella = {"Bella", ["David", "Cindy"]},
	David = {"David", ["Elizabeth"]},
	Cindy = {"Cindy", ["Frank"]},

	% list of people and who they should make friends with
	{[Aaron, Bella, David, Cindy], "David"}.


stop(_SetupData) ->
	friends:cleanup().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_friends({PeopleList, Person}) ->
	lists:foreach(fun({Name, Friends}) ->
		lists:foreach(fun(Friend) ->
			friends:make(Name, Friend)
		end, Friends)
	end, PeopleList),

	DirectFriendList = friends:get_direct_friends(Person),
	[?_assertEqual( lists:sort(DirectFriendList), lists:sort(["Bella", "Elizabeth"]))].
