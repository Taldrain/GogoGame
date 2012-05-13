open Neural_net
open Gene

let cstrNN ni no nl nnl db s =
  let foo = BatDynArray.create() in
  let rec cst_rec yar ni no nl nnl db i s =
    if i = s then
      yar
    else
      (let fool = (new neuralNet ni no nl nnl db) in
         fool#newNeuralNet;
      BatDynArray.add yar fool;
      cst_rec yar ni no nl nnl db (i+1) s)
  in cst_rec foo ni no nl nnl db 0 s 



class assuming_control ni no nl nnl db nbrP nbrC =
  let cstrNN = ni no nl nnl db nbrP nbrC in
object(self)
  val mutable nN = cstrNN
  val mutable input = BatDynArray.create()
  (* array of weight for each node
   * [[w1,w2,w3,...],...]
   *)
  val mutable vecW = BatDynArray.create()
  (* result for each node
   * [n1,n2,...] n is a float
   *)
  val mutable vecA = BatDynArray.create()
  (*(* good result for the input
   * [n1,n2,...]
   *)
  val mutable vecN = BatDynArray.create()*)
  val mutable nbrCycle = nbrC
  val mutable bestW = BatDynArray.create()


  method gBestW = bestW

  (* init the array of input
   * ex: [((i1, i2, i3)(*, out*)),...]
   * and the good result -> Nope
   *)
  method read = 0

  method fBestW =
    let rec f_rec n i s =
      if i = s then
        (BatDynArray.get vecW n)
      else
        (if n < (truncate(BatDynArray.get vecA i)) then
          (let foo = (truncate(BatDynArray.get vecA i)) in
            f_rec foo (i+1) s)
        else
            f_rec n (i+1) s)
    in f_rec 0 0 (BatDynArray.length vecA);
       ()


  method mW =
    let foo = BatDynArray.create() in
    let rec mW_rec yar fool i s =
      if i = s then
        fool
      else
        (let bar = (BatDynArray.get yar i)#getWeight in
           BatDynArray.add fool bar;
        mW_rec yar fool (i+1) s)
    in vecW <- mW_rec nN foo 0 (BatDynArray.length nN)


  method cycleNN =
    let rec cycleOne note i s =
      if i = s then
        (note /. (float(BatDynArray.length input)))
      else
        (let foo = (BatDynArray.get input i) in
         let bar = (BatDynArray.get nN i)#update foo in
         let fool = note +. (BatDynArray.last bar) in
           cycleOne fool (i+1) s)
    in
    let rec cycleC yar i s =
      if i = s then
        yar
      else
        (let foo = cycleOne 0. 0 (BatDynArray.length input) in
           BatDynArray.add yar foo;
        cycleC yar (i+1) s)
    in
    let foo = cycleC (BatDynArray.create()) 0 (BatDynArray.length nN) in
      vecA <- foo

  method putvWeight =
    let rec nihao yar ar i s =
      if i != s then
        (let foo = BatDynArray.get yar i in
         let bar = BatDynArray.get ar i in
           foo#putWeight bar;
         nihao yar ar (i+1) s)
    in nihao nN vecW



  method cycleGene = (*TOFIX*)
    let gen = new geneAlgo (BatDynArray.length nN) 2. 2. (BatDynArray.length vecW)
                2. vecW vecA in
      gen#cycle;
      vecW <- gen#gW;
      self#putvWeight;
      ()

  method complete =
    let rec doIt i s =
      if i = (s - 1) then
        self#cycleNN
      else
        (if i = s then
          self#fBestW
        else
          (self#cycleNN;
           self#cycleGene))
    in doIt 0 nbrCycle
end

