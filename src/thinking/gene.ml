open Common


class genome (w: float BatDynArray.t) (f: float) =
object
  val mutable vecWeight = w
  val mutable fitness = f

  method gVecWeight = vecWeight
  method gFitness = fitness
end

let constructG cl pop c f = 
  let rec vPS yar i s cL c f =
    if i = s then
      yar
    else
       (BatDynArray.add yar (new genome (BatDynArray.get c i)
                               (BatDynArray.get f i));
       vPS yar (i+1) s cL c f)
  in vPS (BatDynArray.create()) 0 pop cl c f

class geneAlgo populos mRate cRate chromoLength mP mW mF = 
  let crVpop = constructG chromoLength populos mW mF in
object(self)
  val mutable vPopu = crVpop
  val mutable popSize = populos
  val mutable wChromo = chromoLength
  val mutable totalFitness = 0.
  val mutable bestFitness = 0.
  val mutable averageFitness = 0.
  val mutable worstFitness = max_float
  val mutable bestGenome = 0
  val mutable mutRate = mRate
  val mutable crsRate = cRate
  val mutable cntGeneration = 0
  val mutable maxPerturbation = mP

  method gPopu = vPopu

  method sVPopu s = vPopu <- s
  method sTotalFitness s = totalFitness <- s
  method sBestFitness s = bestFitness <- s
  method sWorstFitness s = worstFitness <- s
  method sAverageFitness s = averageFitness <- s
  method sCntGeneration s = cntGeneration <- s
  method sBestGenome s = bestGenome <- s

  method gW =
    let foo = BatDynArray.create() in
    let rec gw_rec yar i s =
      if i = s then
        yar
      else
        (let bar = (BatDynArray.get vPopu i)#gVecWeight in
           BatDynArray.add yar bar;
        gw_rec yar (i+1) s)
    in gw_rec foo 0 ((BatDynArray.length vPopu)-1)

  method plusGene =
    self#sCntGeneration (1+cntGeneration)

  method reset =
    self#sTotalFitness 0.;
    self#sBestFitness 0.;
    self#sWorstFitness max_float;
    self#sAverageFitness 0.;

  (* deal with it *)
  method sort pop =
    let ar = BatDynArray.to_array pop in
      Array.sort compare ar;
    BatDynArray.of_array(ar)
      
  (* a revoir, bien crade *)
  method fndWBAT =
    self#sTotalFitness 0.;
    let h = ref 0. and low = ref max_float in
    let rec thr hf low i s vPop =
      if i != s then
        (if ((BatDynArray.get vPop i)#gFitness < !hf) then
           hf := (BatDynArray.get vPop i)#gFitness;
           self#sBestGenome  i;
           self#sBestFitness !hf);
        (if ((BatDynArray.get vPop i)#gFitness < !low) then
          low := (BatDynArray.get vPop i)#gFitness;
           self#sWorstFitness !low);
        self#sTotalFitness (totalFitness +. (BatDynArray.get vPop i)#gFitness);
        thr hf low (i+1) s vPop
    in thr h low 0 popSize vPopu;
    self#sAverageFitness (totalFitness /. (float popSize))

  method mutate chr =
    let rec parseChr yar i s =
      if i = s then
        yar
      else
        ((if randF() < mutRate then
          let foo = (BatDynArray.get yar i) in
          BatDynArray.set yar i (foo +. randC() *. maxPerturbation));
        parseChr yar (i+1) s)
    in parseChr chr 0 ((BatDynArray.length chr)-1)


  (* bof, ... *)
  method rolling =
    let sl = randF() *. totalFitness and
            theChosenOne = (new genome (BatDynArray.create ()) 0.) in
    let rec gT yar tCO apt sL i s =
      if i = s then
        tCO
      else
        (let foo = apt +. ((BatDynArray.get yar i)#gFitness) in
           if (foo >= sL) then
             gT yar (BatDynArray.get yar i) foo sL s s
           else
             gT yar tCO foo sL (i+1) s)
    in gT vPopu theChosenOne 0. sl 0 popSize


  method crossOver mom dad child1 child2 =
    if (randF() > crsRate) || (mom == dad) then
      (child1, child2)
    else
      let cp = randI (wChromo - 1) in
      let rec off m d c1 c2 i s =
        if i != s then
          (BatDynArray.add c1 (BatDynArray.get m i);
           BatDynArray.add c2 (BatDynArray.get d i);
           off m d c1 c2 (i+1) s)
      in off mom dad child1 child2 0 cp;
      let rec on m d c1 c2 i s =
        if i != s then
          (BatDynArray.add c1 (BatDynArray.get d i);
           BatDynArray.add c2 (BatDynArray.get m i);
           off m d c1 c2 (i+1) s)
      in on mom dad child1 child2 cp ((BatDynArray.length mom)-1);
      (child1, child2)
    
  method cycle =
    (*self#reset;*)
    self#sort vPopu;
    self#fndWBAT;
    let nPop = BatDynArray.create() in
    let rec finalSolution oP nP =
      if (BatDynArray.length nP >= BatDynArray.length oP) then
        nP
      else
        (let fmom = self#rolling
         and fdad = self#rolling
         and child1 = BatDynArray.create()
         and child2 = BatDynArray.create() in
        let children = (self#crossOver fmom#gVecWeight fdad#gVecWeight
                          child1 child2) in
        let c1 = self#mutate (fst children) in
        let c2 = self#mutate (snd children) in
        BatDynArray.add nP (new genome c1 0.);
        BatDynArray.add nP (new genome c2 0.);
        self#plusGene;
        finalSolution oP nP)
    in finalSolution vPopu nPop;
    self#sVPopu nPop;
    nPop

end

