-module(btree).
-include("interface.hrl").
-export([start/0, find/2, tree_of_squares/1, insert/3]).

start() ->
      read_dict("dict.dic").

read_dict(File) ->
  Bin = file:read_file(File),
  {ok, BinList} = Bin,

  DL = binary:split(BinList, [<<"\n">>], [global]),
  AVLDictionary = parse(DL),

  io:format("~p~n", [AVLDictionary]),
  await_request(AVLDictionary).

  await_request(Dict) -> 
    receive
      {fetch_word, Word, PID} when is_atom(Word) == false ->
        case find(binary:list_to_bin(Word), Dict) of
          not_found -> 
              PID ! word_not_found;
          Definition  -> 
              {ok, Bin} = Definition,
              PID ! binary_to_list(Bin)
        end
    end,
    await_request(Dict).

parse(Tree) -> 
    parse(Tree, nil).

parse([], Tree) -> Tree;

parse([Word,Meaning | TheRest], T1) ->
    T2 = insert(Word, Meaning, T1),
    parse(TheRest, T2).

find(SearchKey, Node) when SearchKey == Node#node.key ->
  {ok, Node#node.val};

find(_, nil) ->
       not_found;

find(SearchKey, Node) when SearchKey < Node#node.key ->
  find(SearchKey, Node#node.left);

find(SearchKey, Node) ->
  find(SearchKey, Node#node.right).


%% tree_of_squares/1 : make a tree where key = val and contains
%% squares of all numbers from 1 to Max
tree_of_squares(Max) ->
	tree_of_squares(Max, nil).


tree_of_squares(0, Tree) -> Tree;

tree_of_squares(Max, Tree) ->
  insert(Max * Max, Max * Max, tree_of_squares(Max - 1, Tree)).

%% insert/3 : return a new tree made from inserting (or updating)
%% a node of Key, Val into existing tree or new tree (nil)
insert(Key, Val, nil) ->
       #node{ key = Key, val = Val };

insert(Key, Val, Node) when Key == Node#node.key ->
       Node#node{ val = Val };

insert(Key, Val, Node) when Key < Node#node.key ->
       balance(Node#node{ left = insert(Key, Val, Node#node.left)});

insert(Key, Val, Node) ->
       balance(Node#node{ right = insert(Key, Val, Node#node.right)}).

%% height/1 : return the height of the tree
height(nil) ->
       0;
height(Node) when Node#node.left == nil, Node#node.right == nil ->
       1;

height(Node) ->
       height(1, Node#node.left, Node#node.right).

height(Acc, Left, Right)
 when Left == nil;
      Right#node.height > Left#node.height ->
       Acc + Right#node.height;

height(Acc, Left, _) ->
       Acc + Left#node.height.


%% balance/1 : return a new tree which is the balanced version of Tree
%% note also that balance is trusted to always return the root height set
%% correctly
balance(nil) ->
  nil;

balance(Node) when Node#node.left == nil, Node#node.right == nil ->
       Node#node{ height = height(Node)};

balance(#node{ left = L, right = R} = Node) when L#node.height ==
R#node.height ->
       Node#node{ height = height(1, L, R)};

balance(#node{ left = L, right = R} = Node)
 when R == nil ;
      L#node.height + 1 > R#node.height ->
       swing_right(Node);


balance(#node{ left = L, right = R} = Node)
 when L == nil ;
      R#node.height + 1 > L#node.height ->
  swing_left(Node).

swing_left(#node{right = #node{ left = RL, right = RR } = Right} = Node)

 when RR == nil ; RR == nil, RL == nil ;
      RL#node.height > RR#node.height ->

       move_left(Node#node{right = move_right(Right)});

swing_left(Tree) ->
       move_left(Tree).

swing_right(#node{left = #node{ left = LL, right = LR} = Left} = Node)

 when LL == nil ; LL == nil, LR == nil ;
      LR#node.height > LL#node.height ->

       move_right(Node#node{ left = move_left(Left)});

swing_right(Tree) ->
       move_right(Tree).


move_left(Node)
 when Node == nil ;
      Node#node.left == nil, Node#node.right == nil ->
       Node;

move_left(#node{ right = #node{ left = RL } = Right} = Node) ->

       OldRoot = Node#node{ right = RL },
       NewRoot = Right#node{ left = OldRoot#node{ height = height(OldRoot)}},
       NewRoot#node{ height = height(NewRoot) }.


move_right(Node)
 when Node == nil ;
      Node#node.left == nil, Node#node.right == nil ->
       Node;

move_right(#node{ left = #node{ right = LR } = Left } = Node) ->

       OldRoot = Node#node{ left = LR},
       NewRoot = Left#node{ right = OldRoot#node{ height = height(OldRoot)}},
       NewRoot#node{ height = height(NewRoot) }.