// (c) MP-I (1998/9-2006/7) and CP (2005/6-2016/7)

module BTree

open Cp
//import Data.Monoid
//import Control.Applicative
//import List

// (1) Datatype definition -----------------------------------------------------

type BTree<'a> = Empty | Node of 'a * (BTree<'a> * BTree<'a>)


let inBTree x = either (konst Empty) Node x

let outBTree x =
     match x with
     | Empty -> i1() 
     | Node (a,(t1,t2)) -> i2(a,(t1,t2)) 

// (2) Ana + cata + hylo -------------------------------------------------------


let baseBTree f g x = (id -|- (f >< (g >< g))) x  

let recBTree f = baseBTree id f         

let rec cataBTree g = g << (recBTree (cataBTree g)) << outBTree 

let rec anaBTree g = inBTree << (recBTree (anaBTree g) ) << g

let hyloBTree a c = cataBTree a << anaBTree c


// (3) Map ---------------------------------------------------------------------

//instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f x = (cataBTree ( inBTree << baseBTree f id )) x 


// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inBTree << (id -|- (id >< swap))) x

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either zero (succ << add << p2)) x


// (4.3) Serialization ---------------------------------------------------------


// (4.3) Serialization ---------------------------------------------------------

let inord a = 
     let join(x,(l,r))=l@[x]@r
     in (either nil join) a

let inordt x = cataBTree inord x                

     
let preord a = 
          let f(x,(l,r))=x::l@r
          in (either nil f) a

let preordt x = cataBTree preord x

     
let postordt a =
          let f(x,(l,r))=l@r@[x]
          in cataBTree (either nil f) a



// (4.4) QuickSort ------------------------------------------------------


let rec part p l = 
     match l with
      | [] -> ([],[])
      | h::t -> if p h then let (s,l) = part p t in (h::s,l) else let s,l = part p t in (s,h::l)          



//let qsep l = 
//     match l with
//      | [] -> Left()
//      | h::t -> let s,l = part (<h) t in Right (h,(s,l))


//let qSort x = (hyloBTree inord qsep) x



// (4.5) Traces ------------------------------------------------

//let tunion(a,(l,r)) x = union (map a:: l) (map a:: r) x 

//let traces x = cataBTree (either (konst [[]]) tunion) x


 
// (4.6) Towers of Hanoi -------------------------------------------------------------

let present x = inord x

let strategy(d,l) = 
     match l with
      | 0 -> i1()
      | n -> i2((n-1,d),((not d,n-1),(not d,n-1)))

let hanoi x = hyloBTree present strategy x


// (5) Depth and balancing (using mutual recursion) --------------------------


let h(a,((b1,b2),(d1,d2))) = (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)
let f((b1,d1),(b2,d2)) = ((b1,b2),(d1,d2))

let baldepth x = cataBTree (either (konst(true,1)) (h<<(id><f))) x

let balBTree x = (p1 << baldepth) x

let depthBTree x = (p2 << baldepth) x