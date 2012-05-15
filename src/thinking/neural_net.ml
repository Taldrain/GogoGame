(* Constructor for the class Neuron
 * -> add a random nbr instead of the current match
 *)
let constructorN n =
  let ar = BatDynArray.create () in
  let rec cstor_rec yar s i =
    if i = s then
      yar
    else
      (BatDynArray.add yar (Common.randC()) ;
      cstor_rec yar s (i+1))
  in cstor_rec ar (n+1) 0


(* Class for one neuron *)
class neuron nbr_input =
  let arrayN = constructorN nbr_input in
object
  val mutable nbrInput = BatDynArray.length arrayN
  val mutable inputArr = arrayN
  method gInputArr = inputArr
  method gNbrInput = nbrInput
end


(* Constructor for the class LayoutNeuron
 *)
let constructorL nbrN nbrIN =
  let ar = BatDynArray.create () in
  let rec cstor_rec yar e s i =
    if i = s then
      yar
    else
     (BatDynArray.add yar (new neuron e);
     cstor_rec yar e s (i+1))
  in cstor_rec ar nbrIN nbrN 0

(* Class for one layout of neuron
 *)
class layoutNeuron nbr_neuron nbr_input_neuron =
  let arrayL = constructorL nbr_neuron nbr_input_neuron in
object
  val mutable nbrNeuron = BatDynArray.length arrayL
  val mutable neuronArr = arrayL
  method gNeuronArr = neuronArr
  method gNbrNeuron = nbrNeuron
end


(* class of the neural network
 *)
class neuralNet nbri nbro nbrh nbrn db =
  let foo = BatDynArray.create () in
object(self)
  val mutable nbrInput = nbri
  val mutable nbrOutput = nbro
  (* nbr intern layout *)
  val mutable nbrILayout = nbrh
  (* nbr neuron per intern layout *)
  val mutable nbrNLayout = nbrn
  (* array for all intern layout with also the output layout *)
  val mutable layoutArr = foo
  val mutable bias = db


  method evalFunction input res =
    (1. /.(1. +. (exp (-.input /. res))))

  (* ! to invoque when a new obj is created ! *)
  method newNeuralNet =
    if (nbrILayout > 0) then
      (BatDynArray.add layoutArr (new layoutNeuron nbrNLayout nbrInput);
      let rec mLayout yar s i =
        if i = s then
          yar
        else
          (BatDynArray.add yar (new layoutNeuron nbrNLayout nbrNLayout);
          mLayout yar s (i+1))
      in mLayout layoutArr (nbrILayout - 1) 0;
      BatDynArray.add layoutArr (new layoutNeuron nbrOutput nbrNLayout))
    else
      BatDynArray.add layoutArr (new layoutNeuron nbrOutput nbrInput)

  (* get the weight of all node, in a DynArr *)
  method getWeight =
    let ar = BatDynArray.create () in
    let rec pWeight yar p k s =
      if k = s then
        yar
      else
        (BatDynArray.add yar (BatDynArray.get p#gInputArr k);
        pWeight yar p (k+1) s)
    in
    let rec pNeuron yar f j s =
      if j = s then
        yar
      else
        (let foo = BatDynArray.get f#gNeuronArr j in
        pWeight yar foo 0 foo#gNbrInput;
        pNeuron yar f (j+1) s)
    in
    let rec pLayout yar i s =
      if i = s then
        yar
      else
        (let foo = BatDynArray.get layoutArr i in
        pNeuron yar foo 0 foo#gNbrNeuron;
        pLayout yar (i+1) s)
    in pLayout ar 0 (nbrILayout + 1)

  (* get the nbr total of weight *)
  (*method getNbrWeight =
    let rec pWeight yar k s =
      if k = s then
        yar
      else
        pWeight (yar+1) (k+1) s
    in
    let rec pNeuron yar f j s =
      if j = s then
        yar
      else
        (let foo = BatDynArray.get f#gNeuronArr j in
        pWeight yar 0 foo#gNbrInput;
        pNeuron yar f (j+1) s)
    in
    let rec pLayout yar i s =
      if i = s then
        yar
      else
        (let foo = BatDynArray.get layoutArr i in
        pNeuron yar foo 0 foo#gNbrNeuron;
        pLayout yar (i+1) s)
    in pLayout 0 0 (nbrILayout + 1)*)

  (* replace the weight with the one in the dynarr t *)
  method putWeight t =
    let rec pWeight p n k s =
      if k != s then
        (BatDynArray.set p#gInputArr k (BatDynArray.get t n);
        pWeight p (n+1) (k+1) s)
    in
    let rec pNeuron f n j s =
      if j != s then
        (let foo = BatDynArray.get f#gNeuronArr j in
        pWeight foo n 0 foo#gNbrInput;
        pNeuron f n (j+1) s)
    in
    let rec pLayout n i s =
      if i != s then
        (let foo = BatDynArray.get layoutArr i in
        pNeuron foo n 0 foo#gNbrNeuron;
        pLayout n (i+1) s)
    in pLayout 0 0 (nbrILayout + 1)


  (* find the output from an input
   * -> Use the correct amount of input !
   *)
  method update input =
    let output = BatDynArray.create () in
      if (BatDynArray.length input != nbrInput) then
        output
      else
      let rec pWeightU netI inputs cW f k s =
        if k = s then
          netI
        else
            (let foo = netI +. (BatDynArray.get f#gInputArr k) *.
                    (BatDynArray.get inputs cW) in
          pWeightU foo inputs (cW+1) f (k+1) s)
      in
      (* simplification possible *)
      let rec pNeuronU inputs outputs p j s =
        if j = s then
          outputs
        else
          (let foo = (BatDynArray.get p#gNeuronArr j)#gNbrInput in
          let bar = pWeightU 0. inputs 0 (BatDynArray.get p#gNeuronArr j) 0 (foo-1)
          in
          let tmp = bar +. 
                   (BatDynArray.get
                      (BatDynArray.get p#gNeuronArr j)#gInputArr
                      (truncate((float foo) -. 1.)) *. (float bias))
          in
          BatDynArray.add outputs (self#evalFunction tmp 1.);
          pNeuronU inputs outputs p (j+1) s)
      in
      let rec pLayoutU inputs outputs i s =
        let rec assign inp out e s n =
          if (n > 0) && (e < s) then
              let foo = BatDynArray.get out e in
               BatDynArray.set inp e foo;
            assign inp out (e+1) s n
        in
        assign inputs outputs 0 ((BatDynArray.length outputs)-1) i;
        let foo = BatDynArray.get layoutArr i in
        pNeuronU inputs outputs foo 0 foo#gNbrNeuron;
        pLayoutU inputs outputs (i+1) s
      in
        pLayoutU input output 0 (nbrILayout)
end

