(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Peter Gifford
* peter.lee.gifford@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)
(* function to increment all items in a list *)
fun f [] = [] (* a *)
  | f (x::xs) = (x + 1) :: (f xs) (* b *);

(* This allows you to look all the way through a list*)
Control.Print.printDepth := 100;

(* creating set data type *)
datatype 'element set = Empty
| Set of 'element * 'element set;    (* Creates data types that are a set of an element followed by set of elements *)

(* isMember function - checks is an element is within a set *)
fun isMember e Empty = false        (* returns false if it is empty *)
|   isMember e (Set(x, xs)) =       (* takes in a comparator and a set then splits up the set *)
    if e=x then true                (* if the comparator is equal to the head element then it returns true *)
    else isMember e xs;             (* Otherwise it moves onto the next value in the list recursively *)

(*Removes all of the doubles in a list before hand*)
fun list2Set [] = Empty                                 (* If there is no list left it sets last element to empty*)
|   list2Set (x::xs) =                                  (*checks first element of list*)
      let fun nest (x,[]) = []                          (* Once first element has been checked against the list it will return empty list and move on*)
          | nest(x,y::ys) = if x=y then nest(x,ys)      (* takes first two vals then if they are equal then you remove the second repeated element*)
                else y::nest(x,ys)                      (* if no equal you keep y in the list while removing it from the next call of the nested function so that the following element can be checked while y is still kept in the final list *)
      in
        Set(x, list2Set(nest(x,xs)))                    (* adds element to the set and uses the nested function to check the next element in the list agains the remaining elements *)
      end;

(*Takes in two sets and returns the union of the two*)
fun union Empty Empty = Empty                                               (* once it has gone through the sets it will stop *)
|   union (Set(x, xs)) set2 = if(isMember x set2) then union xs set2        (* for the first set, checks to see if element is part of the second set. If it is then it skips that number *)
    else Set(x, union xs set2)                                              (* If it is not a repeated number then it adds the vale ot the new set and runs for the next number *)
|   union Empty (Set(y, ys)) = Set(y, union Empty ys);                      (* once the first set has run, the second set is just added to the set because all double values have not been added yet because of the first check *)


(* Takes in two sets and returns the intersection of the values, so all the values that are in both sets *)
fun intersect Empty set2 = Empty                                                              (* This is for the end of the function that will finish off the nes set with Empty. set2 is included because we dont actually need to go through it so it will not be empty *)
|   intersect (Set(x, xs)) set2 = if(isMember x set2) then Set(x, intersect xs set2)          (* Checks to see if x is in the second set. If it is then it adds it. We should only need to run on one list because it is just intersecting values. *)
    else intersect xs set2;                                                                   (* If there is not an intersection then it just checks the next value *)



    (* Simple function to stringify the contents of a Set of characters *)
    fun stringifyCharSet Empty = ""
      | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

    (* Simple function to stringify the contents of a Set of ints *)
    fun stringifyIntSet Empty = ""
      | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

    (* Simple function to stringify the contents of a Set of strings *)
    fun stringifyStringSet Empty = ""
      | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

    (* Simple function that prints a set of integers *)
    fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

    (* Simple function that prints a set of strings *)
    fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

    (* Simple function that prints a set of characters *)
    fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

    list2Set [1, 3, 2];
    list2Set [#"a", #"b", #"c"];
    list2Set [];
    list2Set [6, 2, 2];
    list2Set ["x", "y", "z", "x"];

    (* Question 1 *)
    f [3, 1, 4, 1, 5, 9];

    (* Question 5 *)
    val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
    print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

    (* Question 7 *)
    val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
    print "\nQuestion 7: ";
    print_str quest7;
    print "\n";

    (* Question 9 *)
    print "\nQuestion 9: ";
    print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

    (* Question 10 *)
    print "\nQuestion 10: ";
    print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
