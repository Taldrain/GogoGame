let dBias = (*TODO*)

(* Constructor for the class Neuron
 * -> add a random nbr instead of the current match
 *)
let constructorN n =
  let ar = BatDynArray.create in
  let rec cstor_rec yar s i = match i with
      s -> yar
    | _ -> BatDynArray.add yar i;
           cstor_rec yar s (i+1)
  in cstor_rec ar (n+1) 0


(* Class for one neuron *)
class neuron nbr_input =
  let arrayN = constructorN nbr_input in
object
  val mutable nbrInput = BatDynArray.length arrayN
  val mutable inputArr = arrayN
end


(* Constructor for the class LayoutNeuron
 *)
let constructorL nbrN nbrIN =
  let ar = BatDynArray.create in
  let rec cstor_rec yar e s i = match i with
      s -> yar
    | _ -> BatDynArray.add yar (new neuron e);
           cstor_rec yar e s (i+1)
  in cstor_rec ar nbrIN nbrN 0

(* Class for one layout of neuron
 *)
class layoutNeuron nbr_neuron nbr_input_neuron =
    let arrayL = constructorL nbr_neuron nbr_input_neuron in
object
  val mutable nbrNeuron = BatDynArray.length arrayL
  val mutable neuronArr = arrayL
end


(* class of the neural network
 *)
class neuralNet nbri nbro nbrh nbrn =
  let foo = BarDynArr.create in
object
  val mutable nbrInput = nbri
  val mutable nbrOutput = nbro
  (* nbr intern layout *)
  val mutable nbrILayout = nbrh
  (* nbr neuron per intern layout *)
  val mutable nbrNLayout = nbrn
  (* array for all intern layout with also the output layout *)
  val mutable layoutArr = foo

  (* ! to invoque when a new obj is created ! *)
  val method newNeuralNet =
    if (nbrILayout > 0) then
      BatDynArr.add layoutArr (new layoutNeuron nbrNLayout nbrInput);
      let rec mLayout yar = function
          0 -> yar
        | i -> BatDynArr.add layoutArr (new layoutNeuron nbrNLayout nbrNLayout);
               mLayout yar (i-1)
      in mLayout layoutAr (nbrILayout - 1);
      BatDynArr.add layoutArr (new layoutNeuron nbrOutput nbrNLayout)
    else
      BatDynArr.add layoutArr (new layoutNeuron nbrOutput nbrInput)

  (* get the weight of all node, in a DynArr *)
  val method getWeight =
    let ar = BatDynArr.create in
    let rec pWeight yar p k s = match k with
        s -> yar
      | _ -> BatDynArr.add yar p.inputArr.(k);
             pWeight yar p (k+1) s
    in
    let rec pNeuron yar f j s = match j with
        s -> yar
      | _ -> pWeight yar k.neuronArr.(j) 0 k.neuronArr.(j).nbrInput;
             pNeuron yar f (j+1) s
    in
    let rec pLayout yar i s = match i with
        s -> yar
      | _ -> pNeuron yar layoutArr.(i) 0 layoutArr.(i).nbrNeuron;
             pLayout yar (i+1) s
    in pLayout ar 0 (nbrILayout + 1)

  (* get the nbr total of weight *)
  val method getNbrWeight =
    let rec pWeight yar k s = match k with
        s -> yar
      | _ -> pWeight (yar+1) (k+1) s
    in
    let rec pNeuron yar f j s = match j with
        s -> yar
      | _ -> pWeight yar 0 k.neuronArr.(j).nbrInput;
             pNeuron yar f (j+1) s
    in
    let rec pLayout yar i s = match i with
        s -> yar
      | _ -> pNeuron yar layoutArr.(i) 0 layoutArr.(i).nbrNeuron;
             pLayout yar (i+1) s
    in pLayout 0 0 (nbrILayout + 1)

  (* replace the weight with the one in the dynarr t *)
  val method putWeight t =
    let rec pWeight p n k s = match k with
        s -> ()
      | _ -> p.inputArr.(k) = t.(n)
             pWeight p (n+1) (k+1) s
    in
    let rec pNeuron f n j s = match j with
        s -> ()
      | _ -> pWeight k.neuronArr.(j) n 0 k.neuronArr.(j).nbrInput;
             pNeuron f n (j+1) s
    in
    let rec pLayout n i s = match i with
        s -> ()
      | _ -> pNeuron layoutArr.(i) n 0 layoutArr.(i).nbrNeuron;
             pLayout n (i+1) s
    in pLayout 0 0 (nbrILayout + 1)


  (* find the output from an input *)
  val method update input =
    let output = BatDynArr.create in
      if (BatDynArray.length input != nbrInput) then
        output
      else
      let rec pWeightU netI inputs cW f k s = match k with
          s -> netI
        | _ -> let foo = netI + f.inputArr.(k) * inputs.(cW) in
             pWeightU foo inputs (cW+1) f (k+1) s
      in
      let rec pNeuronU inputs outputs p j s = match j with
          s -> outputs
        | _ -> let foo f.nbrInput in
               let bar = pWeightU 0 inputs 0 p.neuronArr.(j) 0 foo in
               let tmp = bar + p.neuronArr.(j).inputArr.(foo-1) * dBias in
               BarDynArr.add outputs (evalFunction tmp 1.);
               pNeuronU inputs outputs p (j+1)
      in
      let rec pLayoutU inputs outputs i s = match i with
          s -> outputs
        | _ -> if (i>0) then
                  inputs = outputs
               BatDynArr.clear outputs;
               pNeuronU inputs outputs layout.(i) 0 layout.(i).nbrNeuron;
               pLayoutU inputs outputs (i+1) s
      in
        pLayoutU input output 0 (nbrILayout+1)
  
  

  val method evalFunction input res =
    (*(1. /.(1. +. (exp (-.input /. res))))*)

end



