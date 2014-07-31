-module(friends_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

friends_test_() ->
	[{"make friends",
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
	% test that these two friends are on this list
	% sort them just to ensure lists will be in the same order
	[?_assertEqual( lists:sort(DirectFriendList), lists:sort(["Bella", "Elizabeth"]))].
