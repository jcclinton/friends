-module(friends).

-export([init/0, cleanup/0]).
-export([make/2, unmake/2]).
-export([get_direct_friends/1]).


init() ->
	ets:new(friends, [named_table, set, public]).


cleanup() ->
	ets:delete(friends).



% make name1 and name2 friends
% check if friendship aleardy occurs
% if it does not, add friends to each others list
make(Name1, Name2) when is_list(Name1), is_list(Name2) ->
	AreFriends1 = are_friends(Name1, Name2),
	AreFriends2 = are_friends(Name2, Name1),

	if not AreFriends1 ->
			add_friend(Name1, Name2);
		AreFriends1 ->
			ok
	end,

	if not AreFriends2 ->
			add_friend(Name2, Name1);
		AreFriends2 ->
			ok
	end,

	ok.


% remove name1 and name2 from each others friends list
unmake(Name1, Name2) when is_list(Name1), is_list(Name2) ->
	remove_friend(Name1, Name2),
	remove_friend(Name2, Name1).
	

% get flat list of Name's friends
get_direct_friends(Name) ->
	% get tree of friends
	FriendsTree = get_friends(Name),
	% convert it to a flat list
	FriendsList = gb_trees:to_list(FriendsTree),
	% change it to the format we want
	% ie from [{Name, Value}] -> [Name]
	lists:map(fun({FriendName, _}) ->
		FriendName
	end, FriendsList).











%%%%%%%%%%%%%%%%%
%% private


% remove name2 from name1's friend list
remove_friend(Name1, Name2) when is_list(Name1), is_list(Name2) ->

	% check if they are actually friends
	% delete if they are not
	% gb_trees:delete will crash if Name2 is not in Friends
	AreFriends = are_friends(Name1, Name2),
	if AreFriends ->
			Friends = get_friends(Name1),
			NewTree = gb_trees:delete(Name2, Friends),
			store_friends(Name1, NewTree);
		not AreFriends ->
			ok
	end.



% adds name2 to name1 friends list
add_friend(Name1, Name2) when is_list(Name1), is_list(Name2) ->
	Friends = get_friends(Name1),
	NewFriends = gb_trees:insert(Name2, 1, Friends),
	store_friends(Name1, NewFriends).


% checks if name2 is in name1's friend list
are_friends(Name1, Name2) when is_list(Name1), is_list(Name2)->
	Friends = get_friends(Name1),
	% gb_trees:lookup returns none if Name is not in Friends
	case gb_trees:lookup(Name2, Friends) of
		none -> false;
		_ -> true
	end.




store_friends(Name, Friends) when is_list(Name), is_tuple(Friends)->
	ets:insert(friends, {Name, Friends}).

%get friends, create empty tree if it hasnt been created
get_friends(Name) when is_list(Name) ->
	case ets:lookup(friends, Name) of
		[] -> store_empty(Name);
		[{Name, Friends}] -> Friends
	end.

% create and store empty tree
store_empty(Name) when is_list(Name) ->
	Empty = gb_trees:empty(),
	ets:insert_new(friends, {Name, Empty}),
	Empty.
