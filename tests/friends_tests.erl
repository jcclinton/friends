-module(friends_tests).
-include_lib("eunit/include/eunit.hrl").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_friends_test_() ->
	[{"make friends",
	 {setup, fun init1/0, fun stop/1, fun make_friends1/1}}
	].

unmake_friends_test_() ->
	[{"unmake friends",
	 {setup, fun init_unmake1/0, fun stop/1, fun unmake_friends1/1}}
	].

%indirect_friends_test_() ->
	%[{"indirect friends",
	 %{setup, fun init_indirect1/0, fun stop/1, fun indirect_friends1/1}}
	%].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init1() ->
	friends:init(),
	% friends to check
	Aaron = {"Aaron", ["Bella"]},
	Bella = {"Bella", ["David", "Cindy"]},
	David = {"David", ["Elizabeth"]},
	Cindy = {"Cindy", ["Frank"]},

	% list of people and who they should make friends with
	{[Aaron, Bella, David, Cindy], "David"}.


init_unmake1() ->
	friends:init(),
	% friends to check
	Aaron = {"Aaron", ["Bella"]},
	Bella = {"Bella", ["David", "Cindy"]},
	David = {"David", ["Elizabeth"]},
	Cindy = {"Cindy", ["Frank"]},

	% list of people and who they should make friends with
	{[Aaron, Bella, David, Cindy], "David"}.


init_indirect1() ->
	friends:init(),
	% friends to check
	Aaron = {"Aaron", ["Bella"]},
	Bella = {"Bella", ["Cindy"]},
	Cindy = {"Cindy", ["David"]},

	% list of people and who they should make friends with
	{[Aaron, Bella, Cindy], "Aaron"}.


stop(_SetupData) ->
	friends:cleanup().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_friends1({PeopleList, Person}) ->
	make_all_friends(PeopleList),

	DirectFriendList = friends:get_direct_friends(Person),
	% test that these two friends are on this list
	% sort them just to ensure lists will be in the same order
	[?_assertEqual( lists:sort(DirectFriendList), lists:sort(["Bella", "Elizabeth"]))].


unmake_friends1({PeopleList, Person}) ->
	make_all_friends(PeopleList),

	friends:unmake(Person, "Bella"),
	DirectFriendList = friends:get_direct_friends(Person),
	% test that these two friends are on this list
	% sort them just to ensure lists will be in the same order
	[?_assertEqual( lists:sort(DirectFriendList), lists:sort(["Elizabeth"]))].


indirect_friends1({PeopleList, Person}) ->
	make_all_friends(PeopleList),

	IndirectFriendsList = friends:get_indirect(Person, Person),
	% test that these two friends are on this list
	% sort them just to ensure lists will be in the same order
	[?_assertEqual( lists:sort(IndirectFriendsList), lists:sort(["Cindy", "David"]))].



%%%% helpers
make_all_friends(PeopleList) ->
	lists:foreach(fun({Name, Friends}) ->
		lists:foreach(fun(Friend) ->
			friends:make(Name, Friend)
		end, Friends)
	end, PeopleList).
