(**
UCT est un algo de recherche basé sur Monte - Carlo, fait pour le GO

On verra bien ce que ca va donner...
**)

open BatRandom

let seed = BatRandom.self_init ()

let uctk = 1
(* Larger values give uniform search Smaller values give very selective    *)
(* search                                                                  *)


type t_move = int
type node =
  {
    wins: int;
    visits: int ref;
    move: t_move;
    bestNode: node;
    child: node;
    sibling: node;
  }

let uctSelect (n: node) =
  let bestuct = ref 0 and result = ref nil and next = ref n.child
  in
  while !next <> !nil
  do
    let uctvalue =
    (if !next.visits > 0 then
    begin
      let winrate = next.wins / next.visits
      and uct := uctk * sqrt(ln(float_of_int(n.visits)) /.(5.*.(float_of_int(next.visits))));
      in
      winrate + uct;
    end
    else
      (* Always play a random unexplored move first *)
      10000 + (BatRandom.int 1000))
    in

    if uctvalue > bestuct then
    begin
      bestuct := uctvalue;
      result := next;
    end
    else ();

    next := next.sibling;
  done;
  next

let playSimulation (n: node) =
 begin
   if n.visits = 0 then
     randomresult := clone.playRandomGame
   else
   begin
     if n.child = nil then createChildren n else ();

     next := uctSelect(n);

     clone.makeMove(next.move);
     playSimulation(next);
   end;

   incr n.visits;
   updateWin n randomresult;

   if n.child <> nil then
     setBest n;
 end

 let uctSearch () =
 (begin
  root := getNode ();
  nsimulations := 0;
  while (nsimulations < maxSimulations ) do
  begin
    clone.copyState position;
    playSimulation root;
    incr nsimulations;
  end;
  root.bestMove)

PlayRandomGame This method plays a random game and returns the color of the winner

CreateChildren(n: Node); This procedure generates all legal successors in the position and adds those moves as children to the node n.

UpdateWin(n: Node; randomresult: TGameResult); This procedure adds 1 to n.Wins if the randomresult indicates that this is correct.

CopyState(position: TGameState); In order to play a random game the boardstate has to be copied

SetBest(n: Node) This procedure sets n.BestMove to the child with the highest winrate.

Note that the variable randomresult is a global variable. *)




class node (blk,wht) =
object
	val mutable wins = 0
  val mutable visits = 0
  val blacks = blk
  val whites = wht

  val mutable child = None
  val mutable sibling = None

  method visits = visits
  method wins = wins
  method sibling = sibling

  method expand =
    let heap = algoUtils generate_next {blk=blacks;wht=whites;grp=[];shp=[];mov={col=Black;vert={nb=1;letter='A'}};scr=0}
    in
    child <- Some heap

  method update value =
    visits <- visits + 1;
    wins <- wins + value

  method getWinRate =
    if visits > 0
    then (float_of_int wins) /. (float_of_int visits)
    else 0. (* should not happend *)
end

let get_best_child root =
  let rec find best = function
    | None -> bchild
    | Some child -> if child#visits > best#visits then find child child#sibling
      else find best child#sibling
  in
  find root root.child

let uctk = 0.44 (* sqrt (1/5) *)

let uctSelect node =
  let rec select next best_uct res =
    match next with
      | None -> res
      | Some next -> let uctvalue =
    if next#visits > 0 then
      let uct = uctk *. (sqrt (log ((float_of_int node#visits) /. (float_of_int next#visits))))
      in
      next#getWinRate + uct
    else
      10000 + 1000 * (BatRandom.int 1000)
    in
    if uctvalue > best_uct then select next#sibling uctvalue next
    else select next#sibling best_uct res
  in
  select node#child 0 0

type gameState = | Win = 1 | Lose = 0

let playSimulation (n:node) =
  let randomResult = 
  if n#child = None && n#visits < 10 then (* 10 simulations until chilren are expanded (saves memory) *)
  else
    if n#child = None then 
 class Board {

    // return 0 = lose 1 = win for current player to move
    int playSimulation(Node n) {
        int randomresult =0;
        if (n.child == null && n.visits <10) { 
            randomresult = playRandomGame();
        }
        else {
            if (n.child == null)
                createChildren(n);

            Node next = UCTSelect(n); // select a move
            if (next == null) { /* ERROR */ }
            makeMove(next.x, next.y);

            int res = playSimulation(next);
            randomresult = 1 - res;
        }

        n.update(1 - randomresult); // update node (Node - wins are associated with moves in the Nodes)
        return randomresult;
    }

    // generate a move, using the uct algorithm
    Move UCTSearch(int numsim) {
        root = new Node(-1,-1); // init uct tree
        createChildren(root);

        Board clone = new Board();
        for (int i =0; i < numsim; i ++) {
            clone.copyStateFrom(this);
            clone.playSimulation(root);
        }

        Node n = getBestChild(root);
        return new Move(n.x, n.y);
    }

// NOT IMPLEMENTED YET:

    int BOARD_SIZE =19;
    int[][] f = new int[BOARD_SIZE][BOARD_SIZE]; // the board
    int cur_player =1; // player to make next move (1 or 2)

    void makeMove(int x, int y) {
      f[x][y]= cur_player;
      cur_player =3 - cur_player;
    }

    public void makeRandomMove() {
      int x =0;
      int y =0;
      while (true) {
        x = rand.nextInt(BOARD_SIZE);
        y = rand.nextInt(BOARD_SIZE);
        if (f[x][y]==0 && isOnBoard(x, y)) break;
      }
      makeMove(x, y);
    }

    // return 0 = lose 1 = win for current player to move
    int playRandomGame() {
      int cur_player1 = cur_player;
      while (!isGameOver()) {
        makeRandomMove();
      }
      return getWinner() == curplayer1 ? 1 : 0;
    }

    // expand children in Node
    void createChildren(Node parent) {
      Node last = parent;
      for (int i =0; i < BOARD_SIZE; i ++)
        for (int j =0; j < BOARD_SIZE; j ++)
          if (isOnBoard(i, j) && f[i][j]==0) {
            Node node = new Node(i, j);
            if (last == parent) last.child = node;
                         else last.sibling = node;
            last = node;
          }
    }

    void copyStateFrom(Board b) {

    }

 } /* END: class Board */











