(*Datatypes*)
type stackType = Int of int | String of string | Name of string | Bool of bool | Error of string | Unit of stackType * stackType | Funct of string * string * string * string list (*Function type, functon name, function arg, list of commands in function *)


let interpreter ( (input : string), (output : string )) : unit =

    let ic = open_in input in
    let oc = open_out output in
    


    let rec loop_read acc =
      try 
          
          let l = input_line ic in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in
    
    let file_write (output: string) = Printf.fprintf oc "%s\n" output in

    (*Contains all the commands in list format*)
    let ls_str = loop_read [] in

    (*Check if value of bindList is there, if it is return value *)
    let rec checkBind (name: string) (bindList: stackType list) (functListList: 'a list) = 
        match bindList with
            [] ->  let x = Error(":error:") in x 
            |hd::tl ->
                match hd with
                    Unit (a,b) ->
                        (match a with
                            Name c ->
                                if c = name == true then b
                                else checkBind (name) (tl) (functListList)
                            |_ -> checkBind(name)(tl) (functListList))
                    |_-> checkBind(name)(tl) (functListList) in
                        
    (*Check all the bindLists*)
    let rec checkBindLists (name: string) (bindListList: 'a list) (functListList: 'a list) =
        match bindListList with
            [] -> let x = Error(":error:") in x
            |hd::tl ->
                let currList = checkBind(name) (hd) (functListList) in
                    match currList with
                        Error x -> checkBindLists(name)(tl) (functListList)
                        |_ -> currList in

    let rec checkFun(name: string)(bindListList: 'a list)(functList : stackType list) = 
        match functList with
            [] -> let x = Error(":error:") in x 
            |hd::tl -> 
                match hd with
                    Funct(z,a,b,c) ->
                        if(a = name == true) then hd
                        else checkFun(name)(bindListList)(tl)
                    |_ -> checkFun(name)(bindListList)(tl) in
            
    let rec checkFunLists (name: string) (bindListList : 'a list) (functListList) = 
        match functListList with
            [] -> let x = Error(":error:") in x
            |hd::tl ->
                let currList = checkFun(name)(bindListList)(hd) in
                    match currList with
                        Error x -> checkFunLists(name)(bindListList)(tl)
                        |_ -> currList in
                        
 
    let push (str: string ) ( stack: stackType list) (bindListList : 'a list) (functListList: 'a list) : stackType list = 
        let retVal = stack in
        (*Pushing ints*)
        if(String.get str 0 == '-' && String.get str 1 == '0') then let x = Int(int_of_string(str)) in retVal @ [x]
        else if(String.get str 0 == '-' || String.get str 0 == '0' || String.get str 0 == '1' || String.get str 0 == '2' || String.get str 0 == '3' || String.get str 0 == '4' || String.get str 0 == '5' || String.get str 0 == '6' || String.get str 0 == '7' || String.get str 0 == '8' || String.get str 0 == '9') then
            (*Handling illegal name like 3b *)
            if(String.contains str 'A' || String.contains str 'B' || String.contains str 'C' || String.contains str 'D' || String.contains str 'E' || String.contains str 'F' || String.contains str 'G' || String.contains str 'H' || String.contains str 'I' || String.contains str 'J' || String.contains str 'K' || String.contains str 'L' || String.contains str 'M' || String.contains str 'N' || String.contains str 'O' || String.contains str 'P' || String.contains str 'Q' || String.contains str 'R' || String.contains str 'S' || String.contains str 'T' || String.contains str 'U' || String.contains str 'V' || String.contains str 'W' || String.contains str 'X' || String.contains str 'Y' || String.contains str 'Z' || String.contains str 'a' || String.contains str 'b' || String.contains str 'c' || String.contains str 'd' || String.contains str 'e' || String.contains str 'f' || String.contains str 'g' || String.contains str 'h' || String.contains str 'i' || String.contains str 'j' || String.contains str 'k' || String.contains str 'l' || String.contains str 'm' || String.contains str 'n' || String.contains str 'o' || String.contains str 'p' || String.contains str 'q' || String.contains str 'r' || String.contains str 's' || String.contains str 't' || String.contains str 'u' || String.contains str 'v' || String.contains str 'w' || String.contains str 'x' || String.contains str 'y' || String.contains str 'z' || String.contains str '.') then
            let x = Error(":error:") in retVal @ [x]
            else
                let x = Int(int_of_string(str)) in 
                retVal @ [x]
        (*Pushing names*)
        else if (String.get str 0 == '_' || String.get str 0 == 'A' || String.get str 0 == 'B' || String.get str 0 == 'C' || String.get str 0 == 'D' || String.get str 0 == 'E' || String.get str 0 == 'F' || String.get str 0 == 'G' ||String.get str 0 == 'H' ||String.get str 0 == 'I' ||String.get str 0 == 'J' ||String.get str 0 == 'K' ||String.get str 0 == 'L' ||String.get str 0 == 'M' ||String.get str 0 == 'N' ||String.get str 0 == 'O' ||String.get str 0 == 'P' ||String.get str 0 == 'Q' ||String.get str 0 == 'R' ||String.get str 0 == 'S' ||String.get str 0 == 'T' ||String.get str 0 == 'U' ||String.get str 0 == 'V' ||String.get str 0 == 'W' ||String.get str 0 == 'X' ||String.get str 0 == 'Y' ||String.get str 0 == 'Z' ||String.get str 0 == 'a' ||String.get str 0 == 'b' ||String.get str 0 == 'c' ||String.get str 0 == 'd' ||String.get str 0 == 'e' ||String.get str 0 == 'f' ||String.get str 0 == 'g' ||String.get str 0 == 'h' ||String.get str 0 == 'i' ||String.get str 0 == 'j' ||String.get str 0 == 'k' ||String.get str 0 == 'l' ||String.get str 0 == 'm' ||String.get str 0 == 'n' ||String.get str 0 == 'o' ||String.get str 0 == 'p' ||String.get str 0 == 'q' ||String.get str 0 == 'r' ||String.get str 0 == 's' ||String.get str 0 == 't' ||String.get str 0 == 'u' ||String.get str 0 == 'v' ||String.get str 0 == 'w' ||String.get str 0 == 'x' ||String.get str 0 == 'y' ||String.get str 0 == 'z') then
            let x = Name(str) in retVal @ [x]
        (*Pushing booleans*)
        else if(str = ":true:" == true) then
            let x = Bool(true) in retVal @ [x]
        else if(str = ":false:" == true) then   
            let x = Bool(false) in retVal @ [x]
        (*Pushing errors*)
        else if(str = ":error:" == true) then
            let x = Error(str) in retVal @ [x]
        (*Pushing unit*)
        else if(str = ":unit:" == true) then
            let str = "Empty" in let strType = String(str) in let x = Unit(strType, strType) in retVal @ [x]
        else retVal in           

    let pushString(str:string)(stack: stackType list) (bindListList : 'a list) (functListList: 'a list) = 
        let retVal = stack in
        let x = String.sub str 0 (String.length str) in 
        let y = String(x) in
        retVal @ [y] in

    (*Popping*)
    let pop(stack: stackType list) (bindListList : 'a list) (functListList: 'a list) =
        let retVal = stack in
        match retVal with
            [] -> push(":error:")(retVal) (bindListList) (functListList)
            | hd::tl -> List.rev tl in

    (*Add/Sub/Mul/Div/Rem*)     
    let rec operation (stack: stackType list) (f: int-> int -> int) (operateOn: int list) (originalList: stackType list)(bindListList : 'a list) (functListList: 'a list) = 
        let retVal = stack in
        match retVal with
            []->
                if List.length operateOn != 2 then let err = Error(":error:") in originalList @ [err]
                else 
                     (try let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Int(newVal) in retVal @ [x]
                     with 
                        | Division_by_zero -> let revList = List.rev originalList in let err = Error(":error:") in revList @ [err]
                        | _ -> let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Int(newVal) in retVal @ [x])
            |hd::tl -> 
                if List.length operateOn != 2 then
                    match hd with
                        Int x -> let operateOn = operateOn @ [x] in operation (tl) (f) (operateOn)(originalList ) (bindListList) (functListList)
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                Int x -> let operateOn = operateOn @ [x] in operation (tl) (f) (operateOn)(originalList ) (bindListList)(functListList)
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_  -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList
                else 
                    (try let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Int(newVal) in let revList = List.rev retVal in revList @ [x]
                    with
                        | Division_by_zero -> let revList = List.rev originalList in let err = Error(":error:") in revList @ [err]
                        | _ ->
                            let newVal = f (    List.nth operateOn 0) (List.nth operateOn 1) in
                            let x = Int(newVal) in let revList = List.rev retVal in revList @ [x]) in 
    
    (*Cat*) 
    let rec stringLogic(stack: stackType list) (f: string -> string -> string) (operateOn: string list) (originalList: stackType list) (bindListList : 'a list) (functListList: 'a list)  =
        let retVal = stack in
        match retVal with
            [] ->
                if List.length operateOn != 2 then let err = Error(":error:") in originalList @ [err]
                else let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = String(newVal) in retVal @ [x]
            |hd::tl ->
                if List.length operateOn != 2 then
                    match hd with 
                        String x -> let operateOn = operateOn @ [x] in stringLogic (tl) (f) (operateOn)(originalList) (bindListList) (functListList)
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                String x -> let operateOn = operateOn @ [x] in stringLogic (tl) (f) (operateOn)(originalList ) (bindListList) (functListList)
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList
                else 
                    let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = String(newVal) in 
                    let revList = List.rev retVal in revList @ [x] in

    (*And/Or*)
    let rec boolLogic(stack: stackType list) (f: bool -> bool -> bool) (operateOn: bool list) (originalList: stackType list) (bindListList : 'a list) (functListList: 'a list)=
        let retVal = stack in
        match retVal with
            [] ->
                if List.length operateOn != 2 then let err = Error(":error:") in originalList @ [err]
                else let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Bool(newVal) in retVal @ [x]
            |hd::tl ->
                if List.length operateOn != 2 then
                    match hd with 
                        Bool x -> let operateOn = operateOn @ [x] in boolLogic (tl) (f) (operateOn)(originalList) (bindListList) (functListList)
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                Bool x -> let operateOn = operateOn @ [x] in boolLogic (tl) (f) (operateOn)(originalList ) (bindListList) (functListList)
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList
                else 
                    let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Bool(newVal) in 
                    let revList = List.rev retVal in revList @ [x] in    
    (*Not*)                      
    let rec boolLogicUnary(stack: stackType list) (f: bool -> bool) (operateOn: bool list) (originalList: stackType list) (bindListList : 'a list) (functListList: 'a list) = 
        let retVal = stack in
        match retVal with
            [] ->
                if List.length operateOn != 1 then let err = Error(":error:") in originalList @ [err]
                else let newVal = f(List.nth operateOn 0) in let x = Bool(newVal) in retVal @ [x]
            |hd::tl ->
                if List.length operateOn != 1 then
                    match hd with
                        Bool x -> let operateOn = operateOn @ [x] in boolLogicUnary (tl) (f) (operateOn) (originalList) (bindListList) (functListList)
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                Bool x -> let operateOn = operateOn @ [x] in boolLogicUnary (tl) (f) (operateOn)(originalList ) (bindListList) (functListList)
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList
                else 
                    let newVal = f (List.nth operateOn 0) in let x = Bool(newVal) in
                    let revList = List.rev retVal in revList @ [x] in 

    (*if*)
    let rec iff (stack: stackType list) (operateOn: stackType list) (originalList: stackType list) (bindListList: 'a list) (functListList: 'a list) = 
        let retVal = stack in
        match retVal with
            [] ->
                if List.length operateOn != 3 then let err = Error(":error:") in [err] @ originalList 
                else
                    (match List.nth operateOn 2 with
                        Bool x -> 
                            if x == true then
                                let newVal = List.nth operateOn 0 in [newVal] @ retVal 
                            else
                                let newVal = List.nth operateOn 1 in [newVal] @ retVal 
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList) (functListList) in
                            (match bindVal with
                                Bool x -> 
                                    if x == true then
                                        let newVal = List.nth operateOn 0 in [newVal] @ retVal
                                    else
                                        let newVal = List.nth operateOn 1 in [newVal] @ retVal
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in [err] @ originalList)
            |hd::tl ->
                if List.length operateOn != 3 then
                    let operateOn = operateOn @ [hd] in iff(tl)(operateOn)(originalList)(bindListList) (functListList)
                else 
                    let rev = List.rev retVal in
                    (match List.nth operateOn 2 with
                        Bool x -> 
                            if x == true then
                               let newVal = List.nth operateOn 0 in rev @ [newVal]
                            else
                               let newVal = List.nth operateOn 1 in rev @ [newVal]
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                Bool x -> 
                                    if x == true then
                                        let newVal = List.nth operateOn 0 in rev @ [newVal]
                                    else
                                        let newVal = List.nth operateOn 1 in rev @ [newVal]
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in [err] @ originalList) in


    (*Equal, LessThan*)
    let rec intLogic(stack: stackType list) (f: int -> int -> bool) (operateOn: int list) (originalList: stackType list) (bindListList: 'a list) (functListList: 'a list)  =
        let retVal = stack in
        match retVal with
            [] ->
                if List.length operateOn != 2 then let err = Error(":error:") in originalList @ [err]
                else let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Bool(newVal) in retVal @ [x]
            |hd::tl ->
                if List.length operateOn != 2 then
                    match hd with 
                        Int x -> let operateOn = operateOn @ [x] in intLogic (tl) (f) (operateOn)(originalList) (bindListList) (functListList)
                        |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList) (functListList) in
                            (match bindVal with
                                Int x -> let operateOn = operateOn @ [x] in intLogic (tl) (f) (operateOn)(originalList ) (bindListList) (functListList)
                                |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList)
                        |_ -> let err = Error(":error:") in let originalList = [err] @ originalList in List.rev originalList
                else 
                    let newVal = f (List.nth operateOn 0) (List.nth operateOn 1) in let x = Bool(newVal) in 
                    let revList = List.rev retVal in revList @ [x] in
                    

    let rec updateBind(value: stackType) (bindListHd: stackType list) (newBindListHd: stackType list) (bindListList: 'a list) (functListList: 'a list) = 
        let valName = 
            match value with
                Unit(a,b) ->
                    (match a with
                        Name c -> c
                        |_ -> "Should never happen")
                |_ -> "Should never happen" in
            match bindListHd with
                [] -> bindListList
                |hd::tl ->  
                    match hd with
                        Unit(a,b) ->
                            (match a with
                                Name c ->
                                    if valName = c == true then
                                        let bindListList = [newBindListHd @ [value] @ tl] @ List.tl bindListList in bindListList
                                    else
                                        let newBindListHd = newBindListHd @ [hd] in updateBind (value) (tl)(newBindListHd)(bindListList)(functListList)
                                |_ -> bindListList )
                        |_ -> bindListList in


    let rec bind(stack: stackType list) (operateOn: stackType list) (originalList: stackType list) (bindListList: 'a list)(functListList: 'a list) = 
        let revList = List.rev stack in
        match revList with
            [] -> bindListList
            |hd::tl ->
                (match hd with
                    Unit(a,b) ->    
                        (match a with
                            Name v ->
                                let bindExist = checkBind(v)(List.nth bindListList 0) (functListList) in
                                (match bindExist with
                                    Error x ->
                                        (match bindListList with
                                            [] -> bindListList
                                            |hdd::tll -> let updatehd = hdd @ [hd] in [updatehd] @ tll)
                                    |_ -> let empty = [] in updateBind (hd) (List.nth bindListList 0) (empty) (bindListList) (functListList))
                            |_ -> bindListList)
                    |_ -> bindListList) in

    (*Only adding elements to stack :unit: don't change bindlist *)
    let rec stackbind(stack: stackType list) (operateOn : stackType list) (originalList: stackType list) (bindListList: 'a list) (functListList: 'a list) = 
        let retVal = stack in
        match retVal with
            [] -> 
                if List.length operateOn != 2 then let err = Error(":error:") in originalList @ [err]
                else let bindVal = Unit(List.nth operateOn 1, List.nth operateOn 0) in retVal @ [bindVal] 
            |hd::tl ->
                if List.length operateOn != 2 then
                    if List.length operateOn == 0 then 
                        (match hd with
                            Name x -> let check = checkBindLists(x)(bindListList)(functListList) in
                                (match check with
                                    Error x -> let err = Error(":error:") in let revO = List.rev originalList in revO @ [err]
                                    |_ -> let operateOn = operateOn  @ [check] in stackbind(tl)(operateOn)(originalList)(bindListList)(functListList))(*Check if name is bound to a val...if not add error*)
                            |Error a -> let err = Error(":error:") in let revO = List.rev originalList in revO @ [err]   
                            |_ -> let operateOn = operateOn @ [hd] in stackbind(tl)(operateOn)(originalList)(bindListList)(functListList))
                    else
                        match hd with
                            Name x -> let operateOn = operateOn @ [hd] in stackbind(tl)(operateOn)(originalList)(bindListList)(functListList)  
                            |_ -> let err = Error(":error:") in let revO = List.rev originalList in revO @ [err] (*Adding error here for some reason*)
                else
                    match List.nth operateOn 0 with
                        Name a ->
                            let bindBind = checkBindLists(a) (bindListList)(functListList) in
                                (match bindBind with
                                    Error e ->
                                        let newVal = Unit(List.nth operateOn 1, List.nth operateOn 0) in 
                                        let revList = List.rev retVal in revList @ [newVal]
                                    |_ -> let newVal = Unit(List.nth operateOn 1, bindBind) in 
                                          let revList = List.rev retVal in revList @ [newVal] )
                                        
                        |_ ->
                               let newVal = Unit(List.nth operateOn 1, List.nth operateOn 0) in 
                               let revList = List.rev retVal in revList @ [newVal] in

    let rec addFun(stack: stackType list)(bindListList: 'a list)(functListList: 'a list) (str: string) =
         let split = String.split_on_char ' ' str in
         let emptyList = [] in
         let functVal = Funct(List.nth split 0, List.nth split 1, List.nth split 2, emptyList) in
         match functListList with
            [] -> functListList (*Should never happen *)
            |hd::tl -> let updatehd =  [functVal] @ hd in [updatehd] @ tl in   

    let rec addToFun(stack: stackType list)(bindListList: 'a list)(functListList: 'a list)(str: string) =
        match functListList with
            [] -> functListList
            |functList1::tl ->
                match functList1 with
                    [] -> functListList
                    |hdd::tll ->
                        match hdd with
                            Funct(z,a,b,c) -> 
                                if(String.contains str '"') then
                                let newc = c @ [str] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl 
                                else let x = String.split_on_char ' ' str in
                                if(List.nth x 0 = "push" == true && z = "fun" == true) then
                                    let checkB = checkBindLists(List.nth x 1)(bindListList)(functListList) in
                                        (match checkB with
                                            Error y -> 
                                                (*Check if it's a function name since it's not a bind name *)
                                             let newc = c @ [str] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                                            |Int y -> let newstr = List.nth x 0 ^ " " ^ string_of_int(y) in let newc = c @ [newstr] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                                            |String y -> let newstr = List.nth x 0 ^ " " ^ y in let newc = c @ [newstr] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                                            |Name y -> let newstr = List.nth x 0 ^ " " ^ y in let newc = c @ [newstr] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                                            |Bool y -> 
                                                (match y with
                                                    true -> let newstr = List.nth x 0 ^ " :true:" in let newc = c @ [newstr] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                                                    |false -> let newstr = List.nth x 0 ^ " :false:" in let newc = c @ [newstr] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl)
                                            |_ -> let newc = c @ [str] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl (*Should never happen *) )
                                else 
                                    let newc = c @ [str] in let addTo = Funct(z,a,b,newc) in [[addTo] @ tll] @ tl
                            |_ -> functListList in

    let neg (stack: stackType list) (bindListList : 'a list)(functListList: 'a list) =
        let retVal = stack in 
        match retVal with
            []-> let err = Error(":error:") in [err] @ stack
            | hd::tl ->
                match hd with
                    Int x -> let x = x * -1 in let newhd = Int(x) in newhd::tl
                    |Name y -> 
                            let bindVal = checkBindLists(y)(bindListList)(functListList) in
                            (match bindVal with
                                Int x -> let x = x * -1 in let newhd = Int(x) in newhd::tl
                                |_ -> let err = Error(":error:") in [err] @ stack)
                    |_ -> let err = Error(":error:") in [err] @ stack in

    let rec swap (stack: stackType list)(operateOn: stackType list)(originalList: stackType list) (bindListList : 'a list) (functListList: 'a list) =
        let retVal = stack in
        match retVal with
            []-> 
                if List.length operateOn != 2 then let err = Error(":error:") in [err] @ originalList
                else [List.nth operateOn 1] @ [List.nth operateOn 0] @ retVal
            |hd::tl ->
                if List.length operateOn != 2 then swap(tl)(operateOn @ [hd])(originalList) (bindListList)(functListList)
                else [List.nth operateOn 1] @ [List.nth operateOn 0] @ retVal in
    
    let rec toString (stack: stackType list) (bindListList : 'a list) (functListList: 'a list) = 
        let retVal = stack in
        match retVal with 
            []-> let err = Error(":error:") in [err] @ stack
            | hd::tl ->
                match hd with
                    Int x -> let asString = string_of_int(x) in let newhd = String(asString) in newhd::tl
                    |String y -> stack
                    |Name z -> let newhd = String(z) in newhd::tl
                    |Bool a -> let asString = ":" ^ string_of_bool(a) ^ ":" in let newhd = String(asString) in newhd::tl
                    |Error b -> let newhd = String(b) in newhd::tl
                    |Unit (c,d) -> let newhd = String(":unit:") in newhd::tl 
                    |Funct(z,e,f,g) -> let newhd = String(":unit:") in newhd::tl in

    (*Adds new bindList to bindListList*)
    let bindLet (stack: stackType list) ( bindListList: 'a list) (functListList: 'a list)  = 
        let newBindList = [] in [newBindList] @ bindListList in 

    (*Pops newest bindList from bindListList *) 
    let endLet(stack: stackType list) (bindListList: 'a list) (functListList: 'a list)= 
        match bindListList with
            [] -> bindListList
            |hd::tl -> tl in

    (*Adds new funcTList to functListList *)
    let functLet(stack: stackType list) (bindListList: 'a list) (functListList: 'a list) = 
        let newFunctList = [] in [newFunctList] @ functListList in

    (*Pops newest functList from functListList *)
    let functEnd(stack: stackType list) (bindListList: 'a list) (functListList: 'a list) =
        match functListList with
            [] -> functListList
            |hd::tl -> tl in

    (*Returns stacktype of top of list*)
    let top (stack: stackType list) (bindListList: 'a list) (functListList: 'a list)= 
        let retVal = stack in
            match retVal with
                [] -> let e = Error(":error:") in e
                |hd::tl -> hd in

    (*Pops all the values up until the let *)
    let rec endf(stack: stackType list) (bindListList: 'a list) (endVal : stackType) (atEnd: bool) (functListList: 'a list)= 
            let retVal = stack in
            match retVal with
                [] -> let err = Error(":error:") in [err] @ retVal
                |hd::tl -> 
                    match hd with
                        String x -> 
                            if atEnd == true then let endVal = top (stack) (bindListList) (functListList)  in endf (stack) (bindListList) (endVal) (false) (functListList)
                            else if (x = "end" == true) then endf (tl) (bindListList) (endVal) (true) (functListList)    
                            else if (x = "let" == true) then tl @ [endVal]
                            else endf(tl)(bindListList) (endVal) (false) (functListList)
                        |_ -> 
                            if atEnd == true then let endVal = top (stack) (bindListList)(functListList)  in endf (stack) (bindListList) (endVal) (false) (functListList)
                            else endf(tl)(bindListList) (endVal) (false)(functListList) in

    (*Pops all the values up until the let in function *)
    let rec endff(stack: stackType list) (bindListList: 'a list) (endVal : stackType) (atEnd: bool) (functListList: 'a list)= 
            let retVal = stack in
            match retVal with
                [] -> let err = Error(":error:") in retVal @ [err]
                |hd::tl -> 
                    match hd with
                        String x -> 
                            if atEnd == true then let endVal = top (stack) (bindListList) (functListList)  in endff (stack) (bindListList) (endVal) (false) (functListList)
                            else if (x = "end" == true) then endff (tl) (bindListList) (endVal) (true) (functListList)    
                            else if (x = "let" == true) then  [endVal] @ tl
                            else endf(tl)(bindListList) (endVal) (false) (functListList)
                        |_ -> 
                            if atEnd == true then let endVal = top (stack) (bindListList)(functListList)  in endff (stack) (bindListList) (endVal) (false) (functListList)
                            else endff(tl)(bindListList) (endVal) (false)(functListList) in

        (*1st function prints, second function pops*)
        let println (stack:stackType list) (bindListList: 'a list) (functListList: 'a list)= 
            let retVal = stack in
            match retVal with
                [] -> let err = Error(":error:") in 
                      [err] @ stack
                |hd::tl ->
                    match hd with   
                        String x -> file_write(x) ; tl
                        |_ -> let err = Error(":error:") in 
                              [err] @ stack in
                            
    let rec addCommands (commands: string list)(arg: string) (value: string) (newCommands: string list) =
        match commands with
            [] -> newCommands
            |hd::tl ->
                let split = String.split_on_char ' ' hd in
                if(hd = "funEnd" == false) then
                    match split with
                        [] -> newCommands
                        |hdd::tll -> 
                            match tll with
                                [] -> let newCommands = newCommands @ [hd] in addCommands (tl)(arg)(value) (newCommands)
                                |hddd::tlll -> 
                                    if (hddd = arg == true) then
                                        let newCommands = newCommands @ [hdd ^ " " ^ value] in addCommands(tl)(arg)(value)(newCommands)
                                    else let newCommands = newCommands @ [hd] in addCommands (tl)(arg)(value) (newCommands)
                else
                    let newCommands = newCommands @ ["funEnd"] in newCommands in
    
    let rec removeCommands (commands: string list)  = 
        match commands with
            [] -> []
            |hd::tl ->
                if(hd = "funEnd" == true) then
                    tl
                else
                    removeCommands(tl) in
        
    (*Function commands change stack*)
    let rec doFunct(stack: stackType list)(bindListList: 'a list)(arg: string)(commands: string list)(functListList: 'a list)(operateOn: stackType list)(newStack: stackType list): (stackType list * 'c list * 'd list) =
        let retVal = stack in
        let paramString =  toString(operateOn)(bindListList)(functListList) in let par =(*This value will replace the arbitrary arg *)
            match List.nth paramString 0  with 
                String a -> a
                |_ -> "Should never happen " in
        match commands with
            [] -> (retVal, bindListList, functListList) (*Should hit return or funEnd before this *)
            |hd::tl -> 
                if (hd = "return" == true) then
                    let newStack = List.rev newStack in
                        match List.nth newStack 0 with
                            Name y -> 
                                let check = checkFunLists(y)(bindListList)(functListList) in
                                    (match check with
                                        Error y -> let return = [List.nth newStack 0] @ retVal in let return = List.rev return in (return, bindListList, functListList)
                                        |_ -> let return = [check] @ retVal in let return = List.rev return in (return, bindListList, functListList))
                            |_ -> let return = [List.nth newStack 0] @ retVal in let return = List.rev return in (return, bindListList, functListList)
                else if(hd = "funEnd" == true) then (retVal, bindListList, functListList)
                else if String.contains hd '"' then
                let x = String.split_on_char '"' hd in
                match x with 
                    [] -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                    | hdd :: tll ->
                        if (hdd = "push " == true) then
                            (*let stack = List.rev stack in *)
                            match tll with
                                [] -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                |hddd::tlll -> let newStack = pushString(hddd)(newStack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                        else doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                else 
                    let x = String.split_on_char ' ' hd in 
                    match x with
                        [] -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                        |hdd::tll ->
                            if(hdd = "push" == true) then
                                (*let newStack = List.rev newStack in *)
                                match tll with
                                    [] -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll ->
                                        if(hddd = arg == true && String.contains arg '"') then let hddd = par in
                                        let newStack = pushString(hddd)(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                        else if(hddd = arg == true) then let hddd = par in 
                                        let newStack = push(hddd)(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                        else let newStack = push(hddd)(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "pop" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = pop(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "add" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let add x y = x + y in let emptyList = [] in let newStack = operation(newStack)(add)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "sub" == true) then
                                let newStack = List.rev newStack in 
                                match tll with
                                    [] -> let sub x y = y - x in let emptyList = [] in let newStack = operation(newStack)(sub)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "mul" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let mul x y = x * y in let emptyList = [] in let newStack = operation(newStack)(mul)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)  
                            else if(hdd = "div" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let div x y = y / x in let emptyList = [] in let newStack = operation(newStack)(div)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "rem" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let rem x y = y mod x in let emptyList = [] in let newStack = operation(newStack)(rem)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "neg" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = neg(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "swap" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let emptyList = [] in let newStack = swap(newStack)(emptyList)(stack)(bindListList)(functListList) in let newStack = List.rev newStack in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "toString" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = toString(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "println" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = println(newStack)(bindListList)(functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "cat" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] ->  let cat x y = y ^ x in let emptyList = [] in let newStack = stringLogic(newStack)(cat)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "and" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let a x y = x && y in let emptyList = [] in let newStack = boolLogic(newStack)(a)(emptyList)(stack)(bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "or" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let o x y = x || y in let emptyList = [] in let newStack = boolLogic(newStack)(o)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "not" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let n x = not x in let emptyList = [] in let newStack = boolLogicUnary(newStack)(n)(emptyList)(stack)(bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "equal" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let e x y = x == y in let emptyList = [] in let newStack = intLogic(newStack)(e)(emptyList)(stack)(bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "lessThan" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] ->  let lt x y = x > y in let emptyList = [] in let newStack = intLogic(newStack)(lt)(emptyList)(stack)(bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "bind" == true) then 
                                let newStack = List.rev newStack in let newStack2 = stackbind(newStack)([])(newStack)(bindListList)(functListList) in
                                let newBind = bind(newStack2)([])(newStack2)(bindListList)(functListList) in
                                doFunct(stack)(newBind)(arg)(tl)(functListList)(operateOn)(newStack2)
                            else if(hdd = "if" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let emptyList = [] in let newStack = iff(newStack)(emptyList)(stack) (bindListList) (functListList) in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "let" == true) then 
                                let newStack = List.rev newStack in let lVal = String("let") in let newStack = newStack @ [lVal] in 
                                let newFList =  functLet(newStack)(bindListList)(functListList) in
                                let newBList =  bindLet(newStack)(bindListList) (functListList) in
                                doFunct(stack)(newBList)(arg)(tl)(newFList)(operateOn)(newStack)
                            else if(hdd = "end" == true) then 
                                (*let newStack = List.rev newStack in *)
                                let eVal = String("end") in 
                                let newStack = newStack @ [eVal] in 
                                let newFList = functEnd(newStack)(bindListList)(functListList) in
                                let newBList =  endLet (newStack) (bindListList) (functListList) in
                                let init = Error(":error:") in let newStack = endff(List.rev newStack) (bindListList) (init) (false) (functListList) in doFunct(stack)(newBList)(arg)(tl)(newFList)(operateOn)(List.rev newStack)
                            else if(hdd = "fun" == true) then 
                                let unit = Funct(List.nth x 0, List.nth x 1, List.nth x 2, []) in let newStack = newStack @ [unit] in 
                                let newCommands = addCommands(tl)(arg)(par)([])in 
                                let newFun = Funct(List.nth x 0, List.nth x 1, List.nth x 2, newCommands) in
                                let newFunctList = 
                                    match functListList with
                                        [] -> []
                                        |a::b ->
                                            let a = a @ [newFun] in [a] @ b in
                                let newStackCommands = removeCommands(commands) in
                                doFunct(stack)(bindListList)(arg)(newStackCommands)(newFunctList)(operateOn)(newStack) 
                            else if(hdd = "call" == true) then 
                                let newStack = List.rev newStack in
                                match List.nth newStack 1 with
                                    Name g -> let callCommands = checkFunLists(g)(bindListList)(functListList) in 
                                                (match callCommands with
                                                    Funct(z,a,b,c) -> 
                                                        (match newStack with
                                                            [] -> (stack, bindListList, functListList)
                                                            |hd1::tl1 -> 
                                                                (match tl1 with
                                                                    [] -> (stack, bindListList, functListList)
                                                                    |hd2::tl2 -> let newStack2  = doFunct(List.rev tl2)(bindListList)(b)(c)(functListList)([hd1] @ [hd2])(List.rev tl2) in 
                                                                                 let newStack24 = 
                                                                                    match newStack2 with (a,b,c) -> a in
                                                                                let newBind24 = 
                                                                                    match newStack2 with (a,b,c) -> b in
                                                                                    doFunct(stack)(newBind24)(arg)(tl)(functListList)(operateOn)(newStack24)))
                                                    |_ -> let err = Error(":error:") in let newStack = newStack @ [err] in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack))
                                    |_ -> let err = Error(":error:") in let newStack = newStack @ [err] in doFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "quit" == true) then
                                (retVal, bindListList, functListList)
                            (*Finish writing rest of commands *)
                            else (retVal, bindListList, functListList) in

    (*InOutFunction commands change stack*)
    let rec doIOFunct(stack: stackType list)(bindListList: 'a list)(arg: string)(commands: string list)(functListList: 'a list)(operateOn: stackType list)(newStack: stackType list): (stackType list * 'a list * 'b list) (*Returns list of the new stack and newbinds *) =
        let retVal = stack in
        let paramString =  toString(operateOn)(bindListList)(functListList) in let par =(*This value will replace the arbitrary arg *)
            match List.nth paramString 0  with 
                String a -> a
                |_ -> "Should never happen " in
        match commands with
            [] -> (retVal, bindListList, functListList) (*Should hit return or funEnd before this *)
            |hd::tl -> 
                if (hd = "return" == true) then
                    let newStack = List.rev newStack in
                    match List.nth newStack 0 with
                        Name x -> let checkBind = checkBindLists(x)(bindListList)(functListList) in
                                    (match checkBind with
                                        Error x -> 
                                            let checkFun = checkFunLists(x)(bindListList)(functListList) in
                                                (match checkFun with
                                                    Error x -> let return = [List.nth newStack 0] @ retVal in let return = List.rev return in (return, bindListList, functListList)
                                                    |_ -> let return = [checkFun] @ retVal in let return = List.rev return in (return, bindListList, functListList))             
                                        |_ -> let return = [checkBind] @ retVal in let return = List.rev return in (return, bindListList, functListList))
                        |_ -> let return = [List.nth newStack 0] @ retVal in let return = List.rev return in (return, bindListList, functListList)
                else if(hd = "funEnd" == true) then (retVal, bindListList, functListList)
                else if String.contains hd '"' then
                let x = String.split_on_char '"' hd in
                match x with 
                    [] -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                    | hdd :: tll ->
                        if (hdd = "push " == true) then
                            (*let stack = List.rev stack in *)
                            match tll with
                                [] -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                |hddd::tlll -> let newStack = pushString(hddd)(newStack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                        else doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                else 
                    let x = String.split_on_char ' ' hd in 
                    match x with
                        [] -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                        |hdd::tll ->
                            if(hdd = "push" == true) then
                                (*let newStack = List.rev newStack in *)
                                match tll with
                                    [] -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll ->
                                        if(hddd = arg == true && String.contains arg '"') then let hddd = par in
                                        let newStack = pushString(hddd)(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                        else if(hddd = arg == true) then let hddd = par in 
                                        let newStack = push(hddd)(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                        else let newStack = push(hddd)(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "pop" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = pop(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "add" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let add x y = x + y in let emptyList = [] in let newStack = operation(newStack)(add)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "sub" == true) then
                                let newStack = List.rev newStack in 
                                match tll with
                                    [] -> let sub x y = y - x in let emptyList = [] in let newStack = operation(newStack)(sub)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "mul" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let mul x y = x * y in let emptyList = [] in let newStack = operation(newStack)(mul)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)  
                            else if(hdd = "div" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let div x y = y / x in let emptyList = [] in let newStack = operation(newStack)(div)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "rem" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let rem x y = y mod x in let emptyList = [] in let newStack = operation(newStack)(rem)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "neg" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = neg(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "swap" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let emptyList = [] in let newStack = swap(newStack)(emptyList)(stack)(bindListList)(functListList) in let newStack = List.rev newStack in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "toString" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = toString(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "println" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let newStack = println(newStack)(bindListList)(functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "cat" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] ->  let cat x y = y ^ x in let emptyList = [] in let newStack = stringLogic(newStack)(cat)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "and" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let a x y = x && y in let emptyList = [] in let newStack = boolLogic(newStack)(a)(emptyList)(stack)(bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "or" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let o x y = x || y in let emptyList = [] in let newStack = boolLogic(newStack)(o)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "not" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let n x = not x in let emptyList = [] in let newStack = boolLogicUnary(newStack)(n)(emptyList)(stack)(bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "equal" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] -> let e x y = x == y in let emptyList = [] in let newStack = intLogic(newStack)(e)(emptyList)(stack)(bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "lessThan" == true) then
                                let newStack = List.rev newStack in
                                match tll with
                                    [] ->  let lt x y = x > y in let emptyList = [] in let newStack = intLogic(newStack)(lt)(emptyList)(stack)(bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "bind" == true) then 
                                let newStack = List.rev newStack in
                                match tll with
                                [] -> 
                                    let emptyList = [] in 
                                    let newStack = stackbind(newStack)(emptyList)(newStack) (bindListList) (functListList) in 
                                    let newBind = bind(newStack)(emptyList)(newStack)(bindListList)(functListList) in 
                                    doIOFunct(stack)(newBind)(arg)(tl)(functListList)(operateOn)(newStack)
                                |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "if" == true) then
                                let newStack = List.rev newStack in
                                match tll with  
                                    [] -> let emptyList = [] in let newStack = iff(newStack)(emptyList)(stack) (bindListList) (functListList) in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                                    |hddd::tlll -> doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "let" == true) then 
                                let newStack = List.rev newStack in let lVal = String("let") in let newStack = newStack @ [lVal] in 
                                let newFList =  functLet(newStack)(bindListList)(functListList) in
                                let newBList =  bindLet(newStack)(bindListList) (functListList) in
                                doIOFunct(stack)(newBList)(arg)(tl)(newFList)(operateOn)(newStack)
                            else if(hdd = "end" == true) then 
                                (*let newStack = List.rev newStack in *)
                                let eVal = String("end") in 
                                let newStack = newStack @ [eVal] in 
                                let newFList = functEnd(newStack)(bindListList)(functListList) in
                                let newBList =  endLet (newStack) (bindListList) (functListList) in
                                let init = Error(":error:") in let newStack = endff(List.rev newStack) (bindListList) (init) (false) (functListList) in doIOFunct(stack)(newBList)(arg)(tl)(newFList)(operateOn)(List.rev newStack)
                            else if(hdd = "fun" == true) then 
                                let unit = Funct(List.nth x 0, List.nth x 1, List.nth x 2, []) in let newStack = newStack @ [unit] in 
                                let newCommands = addCommands(tl)(arg)(par)([])in 
                                let newFun = Funct(List.nth x 0, List.nth x 1, List.nth x 2, newCommands) in
                                let newFunctList = 
                                    match functListList with
                                        [] -> []
                                        |a::b ->
                                            let a = a @ [newFun] in [a] @ b in
                                let newStackCommands = removeCommands(commands) in
                                doIOFunct(stack)(bindListList)(arg)(newStackCommands)(newFunctList)(operateOn)(newStack)
                            else if(hdd = "call" == true) then 
                                let newStack = List.rev newStack in
                                match List.nth newStack 1 with
                                    Name g -> let callCommands = checkFunLists(g)(bindListList)(functListList) in 
                                                (match callCommands with
                                                    Funct(z,a,b,c) -> 
                                                        (match newStack with
                                                            [] -> (stack, bindListList, functListList)
                                                            |hd1::tl1 -> 
                                                                (match tl1 with
                                                                    [] -> (stack, bindListList, functListList)
                                                                    |hd2::tl2 -> let newStack2  = doFunct(List.rev tl2)(bindListList)(b)(c)(functListList)([hd1] @ [hd2])(List.rev tl2) in 
                                                                                 let newStack24 = 
                                                                                    match newStack2 with (a,b,c) -> a in
                                                                                let newBind24 = 
                                                                                    match newStack2 with (a,b,c) -> b in
                                                                                    doFunct(stack)(newBind24)(arg)(tl)(functListList)(operateOn)(newStack24)))
                                                    |_ -> let err = Error(":error:") in let newStack = newStack @ [err] in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack))
                                    |_ -> let err = Error(":error:") in let newStack = newStack @ [err] in doIOFunct(stack)(bindListList)(arg)(tl)(functListList)(operateOn)(newStack)
                            else if(hdd = "quit" == true) then
                                (retVal, bindListList, functListList)
                            (*Finish writing rest of commands *)
                            else (retVal, bindListList, functListList) in        

    (*Calls commands within funcution *)
    let rec callFunct (stack: stackType list)(originalList: stackType list)(bindListList: 'a list)(functListList: 'a list) (operateOn: stackType list) (newStack: stackType list) (orignalFlist: 'a list): (stackType list * 'c list * 'd list) = (*At the end just returning top of newstack *)
        let retVal = stack in
        match retVal with
            [] -> if List.length operateOn != 2 then let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                  else  (*Have to find if function name is in functionlistlist *)
                    (match functListList with
                        [] -> let err = Error(":erorr:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                        |hd::tl ->
                            (match hd with
                                [] -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                                |hdd::tll ->
                                        (match List.nth operateOn 1 with
                                            Name x ->
                                                let checkB = checkBindLists(x)(bindListList)(functListList) in
                                                    (match checkB with
                                                        Funct(aa,bb,cc,dd) ->
                                                            if(aa = "fun" == true) then doFunct(retVal)(bindListList)(cc)(dd)(orignalFlist)(operateOn)(retVal)
                                                            else doIOFunct(retVal)(bindListList)(cc)(dd)(orignalFlist)(operateOn)(retVal)
                                                        |_ ->
                                                            (match hdd with
                                                                Funct(z,a,b,c) ->
                                                                    let extractString =
                                                                        (match List.nth operateOn 1 with
                                                                            Name x -> x
                                                                            |_ -> "Yeet") in    
                                                                    if(a = extractString == true) then
                                                                    if(z = "fun" == true) then doFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                                    else doIOFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                                    else let tail = [tll] @ tl in callFunct(retVal)(originalList)(bindListList)(tail)(operateOn)(retVal)(orignalFlist)
                                                                |_ -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)))
                                                            

                                            |_ -> 
                                                (match hdd with
                                                    Funct(z,a,b,c) ->
                                                        let extractString =
                                                            (match List.nth operateOn 1 with
                                                                Name x -> x
                                                                |_ -> "Yeet") in    
                                                        if(a = extractString == true) then
                                                        if(z = "fun" == true) then doFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                        else doIOFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                        else let tail = [tll] @ tl in callFunct(retVal)(originalList)(bindListList)(tail)(operateOn)(retVal)(orignalFlist)
                                                    |_ -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)))))

                                    
                            
            |hd:: tl -> 
                if List.length operateOn != 2 then
                    if List.length operateOn == 0 then
                        match hd with
                            Error x -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                            |Name y -> 
                                let checkBind = checkBindLists(y)(bindListList)(functListList) in
                                    (match checkBind with
                                        Error x -> let operateOn = operateOn @ [hd] in callFunct(tl)(originalList)(bindListList)(functListList)(operateOn)(tl)(orignalFlist)
                                        |_ -> let operateOn = operateOn @ [checkBind] in callFunct(tl)(originalList)(bindListList)(functListList)(operateOn)(tl)(orignalFlist)) 
                            |_ -> let operateOn = operateOn @ [hd] in callFunct(tl)(originalList)(bindListList)(functListList)(operateOn)(tl)(orignalFlist)
                    else 
                        (match hd with
                            Name x -> let operateOn = operateOn @ [hd] in callFunct(tl)(originalList)(bindListList)(functListList)(operateOn)(tl)(orignalFlist)
                            |Funct(d,f,g,h) -> 
                                let operateOn = operateOn @ [hd] in
                                if(d = "fun" == true) then 
                                    doFunct(tl)(bindListList)(g)(h)(orignalFlist)(operateOn)(tl)
                                else doIOFunct(tl)(bindListList)(g)(h)(orignalFlist)(operateOn)(tl)
                            |_ -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList))
                else 
                    (match functListList with
                        [] -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                        |hd::tl ->
                            (match hd with
                                [] -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)
                                |hdd::tll ->
                                    (match List.nth operateOn 1 with
                                            Name x ->
                                                let checkB = checkBindLists(x)(bindListList)(functListList) in
                                                    (match checkB with
                                                        Funct(aa,bb,cc,dd) ->
                                                            if(aa = "fun" == true) then doFunct(retVal)(bindListList)(cc)(dd)(orignalFlist)(operateOn)(retVal)
                                                            else doIOFunct(retVal)(bindListList)(cc)(dd)(orignalFlist)(operateOn)(retVal)
                                                        |_ ->
                                                            (match hdd with
                                                                Funct(z,a,b,c) ->
                                                                    let extractString =
                                                                        (match List.nth operateOn 1 with
                                                                            Name x -> x
                                                                            |_ -> "Yeet") in    
                                                                    if(a = extractString == true) then
                                                                    if(z = "fun" == true) then doFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                                    else doIOFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                                    else let tail = [tll] @ tl in callFunct(retVal)(originalList)(bindListList)(tail)(operateOn)(retVal)(orignalFlist)
                                                                |_ -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList)))
                                                            

                                            |_ -> 
                                                (match hdd with
                                                    Funct(z,a,b,c) ->
                                                        let extractString =
                                                            (match List.nth operateOn 1 with
                                                                Name x -> x
                                                                |_ -> "Yeet") in    
                                                        if(a = extractString == true) then
                                                        if(z = "fun" == true) then doFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                        else doIOFunct(retVal)(bindListList)(b)(c)(orignalFlist)(operateOn)(retVal)
                                                        else let tail = [tll] @ tl in callFunct(retVal)(originalList)(bindListList)(tail)(operateOn)(retVal)(orignalFlist)
                                                    |_ -> let err = Error(":error:") in let ret = originalList @ [err] in (ret, bindListList, functListList))))) in
        
        (*Calls commands based on string provided*)
        let callCommands (str: string) (stack: stackType list) (bindListList : 'a list) (functListList: 'a list): stackType list =
            let retVal = stack in
            if String.contains str '"' then
                let x = String.split_on_char '"' str in
                match x with 
                    [] -> retVal
                    | hd :: tl ->
                        if (hd = "push " == true) then
                            match tl with
                                [] -> retVal
                                |hd::tl -> pushString(hd)(retVal) (bindListList) (functListList)
                        else retVal
            else   
                let x = String.split_on_char ' ' str in 
                match x with 
                    [] -> retVal
                    | hd :: tl ->
                        if(hd = "push" == true) then
                            match tl with   
                                [] -> retVal 
                                |hd::tl -> push(hd)(retVal) (bindListList) (functListList)
                        else if(hd = "pop" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> pop(retVal) (bindListList) (functListList)
                                |hd::tl -> List.rev retVal
                        else if(hd = "add" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let add x y = x + y in let emptyList = [] in operation(retVal)(add)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "sub" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let sub x y = y - x in let emptyList = [] in operation(retVal)(sub)(emptyList)(retVal)(bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "mul" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let mul x y = x * y in let emptyList = [] in operation(retVal)(mul)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "div" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let div x y = y / x in let emptyList = [] in operation(retVal)(div)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "rem" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let rem x y = y mod x in let emptyList = [] in operation(retVal)(rem)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "neg" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let retVal = neg(retVal)(bindListList)(functListList) in List.rev retVal   
                                |hd::tl -> retVal
                        else if(hd = "swap" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let emptyList = [] in let retVal = swap(retVal)(emptyList)(retVal)(bindListList)(functListList) in List.rev retVal
                                |hd::tl -> retVal
                        else if(hd = "toString" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let retVal = toString(retVal)(bindListList)(functListList) in List.rev retVal
                                |hd::tl -> retVal
                        else if(hd = "println" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let retVal = println(retVal)(bindListList)(functListList) in List.rev retVal
                                |hd::tl -> retVal
                        else if(hd = "cat" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let cat x y = y ^ x in let emptyList = [] in stringLogic(retVal)(cat)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "and" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let a x y = x && y in let emptyList = [] in boolLogic(retVal)(a)(emptyList)(retVal)(bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "or" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let o x y = x || y in let emptyList = [] in boolLogic(retVal)(o)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "not" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let n x = not x in let emptyList = [] in boolLogicUnary(retVal)(n)(emptyList)(retVal)(bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "equal" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let e x y = x == y in let emptyList = [] in intLogic(retVal)(e)(emptyList)(retVal)(bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "lessThan" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let lt x y = x > y in let emptyList = [] in intLogic(retVal)(lt)(emptyList)(retVal)(bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "bind" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let emptyList = [] in stackbind(retVal)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "if" == true) then
                            let retVal = List.rev retVal in
                            match tl with
                                [] -> let emptyList = [] in iff(retVal)(emptyList)(retVal) (bindListList) (functListList)
                                |hd::tl -> retVal
                        else if(hd = "let" == true) then
                            let retVal = List.rev retVal in let lVal = String("let") in retVal @ [lVal]
                        else if(hd = "end" == true) then
                                let eVal = String("end") in let retValE = retVal @ [eVal] in 
                                let retValE = List.rev retValE in
                                let init = Error(":error:") in endf(retValE) (bindListList) (init) (false) (functListList)
                        else if(hd = "fun" == true) then
                            let emptyList = [] in let unit = Funct(List.nth x 0, List.nth x 1, List.nth x 2, emptyList) in retVal @ [unit]
                        else if(hd = "call" == true) then
                            let emptyList = [] in 
                            let listRev = List.rev retVal in
                            let newStuff = callFunct(listRev)(retVal)(bindListList)(functListList)(emptyList)(retVal)(functListList) in
                            match newStuff with (a,b,c) -> a 
                        else if(hd = "inOutFun" == true) then
                            let emptyList = [] in let unit = Funct(List.nth x 0, List.nth x 1, List.nth x 2, emptyList) in retVal @ [unit]
                        else if(hd = "quit" == true) then retVal
                        else retVal in

        (*Updates bindlist only if bind command is given *)
        let callBindCommands (str: string) (stack: stackType list) (bindListList : 'a list) (functListList: 'a list): 'a list =
            let retVal = bindListList in
             let x = String.split_on_char ' ' str in 
                match x with 
                    [] -> retVal
                    | hd :: tl ->
                        if(hd = "bind" == true) then
                            let emptyList = [] in bind(stack)(emptyList)(stack) (retVal) (functListList)
                        else if (hd = "let" == true) then
                            bindLet(stack)(bindListList) (functListList)
                        else if (hd = "end" == true) then
                            endLet (stack) (bindListList) (functListList)
                        else if(hd = "call" == true) then
                            let emptyList = [] in
                            let newStuff = callFunct(stack)(stack)(bindListList)(functListList)(emptyList)(stack)(functListList) in
                            match newStuff with (a,b,c) -> b
                        else retVal in

        (*Updates functionList only if a funct command is given *)
        let callFunctCommands (str:string) (stack: stackType list) (bindListList: 'a list) (functListList) (declaringFunct: bool) : 'a list =
            let retVal = functListList in
            let x = String.split_on_char ' ' str in 
                match x with
                    [] -> retVal
                    |hd:: tl ->
                        if(hd = "let" == true && declaringFunct == false) then
                            functLet(stack)(bindListList)(functListList)
                        else if(hd = "end" == true && declaringFunct == false) then
                            functEnd(stack)(bindListList)(functListList)
                        else if((hd = "fun" == true || hd = "inOutFun") && declaringFunct == true) then
                            addFun(stack)(bindListList)(functListList)(str)
                        else if(hd = "call" == true && declaringFunct == false) then
                            let newStuff = callFunct(stack)(stack)(bindListList)(functListList)([])(stack)(functListList) in
                            match newStuff with (a,b,c) -> c
                        else
                            addToFun(stack)(bindListList)(functListList)(str) in (*Add fun and other stuff *)

        (*(*Function to write stack to output*)
        let rec writeOut (st: stackType list) (bindList) = 
            match st with   
                [] -> print_string("")
                | hd::tl -> 
                    match hd with 
                        Int x -> let g = file_write(string_of_int(x)) in writeOut (tl) (bindList)
                        |String y -> let g = file_write(y) in writeOut (tl) (bindList)
                        |Name z -> let g = file_write(z) in writeOut (tl) (bindList)
                        |Bool a -> let g = file_write(":" ^ string_of_bool(a) ^ ":") in writeOut (tl) (bindList)
                        |Error b -> let g = file_write(b) in writeOut (tl) (bindList)
                        |Unit (c,d) -> let g = file_write(":unit:") in writeOut (tl) (bindList) 
                        |Funct (z,e,f,g) -> let g = file_write(":unit:") in writeOut (tl) (bindList) in  *)
     

                       


        (*Function to read commands and call appropriate functions*)
        let rec readCommands (str : string list) (stack : stackType list) (bindListList : 'a list) (declaringFunct: bool) (functListList: 'a list) (nested: bool) : unit = 
            match str with
                [] ->  (*let stack =  List.rev stack in writeOut(stack) (bindListList) *) print_string ("")  
                | hd::tl ->
                    let strip = String.trim hd in
                    let x = String.split_on_char ' ' strip in 
                        match x with 
                            [] -> print_string  "" 
                            | hdd :: tll ->     
                                if(hdd = "fun" == true && declaringFunct == false) then    
                                    let newList = callCommands(strip)(stack)(bindListList)(functListList) in
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(true) in
                                    readCommands (tl)(newList)(bindListList)(true) (newFunctList)(nested)
                                else if(hdd = "inOutFun" == true) then
                                    let newList = callCommands(strip)(stack)(bindListList)(functListList) in
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(true) in
                                    readCommands(tl)(newList)(bindListList)(true)(newFunctList)(nested)
                                else if(hdd = "funEnd" == true && nested == false) then 
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(false) in
                                    readCommands (tl)(stack)(bindListList)(false) (newFunctList)(false)
                                else if (hdd = "funEnd" == true && nested == true) then
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(false) in
                                    readCommands (tl)(stack)(bindListList)(true) (newFunctList)(false)
                                else if (hdd = "let" == true && declaringFunct == false) then
                                    let newList = callCommands(strip) (stack)(bindListList)(functListList) in 
                                    let newBindList = callBindCommands (strip)(newList)(bindListList)(functListList) in
                                    let newFunctList = callFunctCommands(strip)(newList)(bindListList)(functListList)(false) in
                                    readCommands(tl) (newList) (newBindList)(false)(newFunctList)(nested)
                                else if (hdd = "end" == true && declaringFunct == false) then
                                    let newList = callCommands(strip) (stack)(bindListList)(functListList) in 
                                    let newBindList = callBindCommands (strip)(newList)(bindListList)(functListList) in
                                    let newFunctList = callFunctCommands(strip)(newList)(bindListList)(functListList)(false) in
                                    readCommands(tl) (newList) (newBindList)(false)(newFunctList)(nested)
                                else if(hdd = "let" == true && declaringFunct == true) then
                                     let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(true) in
                                     readCommands(tl) (stack) (bindListList)(true)(newFunctList)(nested)
                                else if(hdd = "end" == true && declaringFunct == true) then
                                     let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(true) in
                                     readCommands(tl) (stack) (bindListList)(true)(newFunctList)(nested)
                                else if (hd = "call" && declaringFunct == false) then
                                    let newList = callCommands(strip) (stack)(bindListList)(functListList) in 
                                    let emptyList = [] in let newStuff = let newBindList =  callFunct(List.rev stack)(List.rev stack)(bindListList)(functListList)(emptyList)(List.rev stack)(functListList) in
                                        match newBindList with (a,b,c) -> b in
                                    let newFStuff = let newFList = callFunct(List.rev stack)(List.rev stack)(bindListList)(functListList)([])(List.rev stack)(functListList) in
                                        match newFList with (a,b,c) -> c in
                                    readCommands(tl) (newList) (newStuff)(false)(newFStuff)(nested)
                                else if (hdd = "fun" && declaringFunct == true) then
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(false) in
                                    readCommands(tl)(stack)(bindListList)(true)(newFunctList)(true)
                                else if(declaringFunct = false) then
                                    let newList = callCommands(strip) (stack)(bindListList)(functListList) in 
                                    let newBindList = callBindCommands (strip)(newList)(bindListList)(functListList) in
                                    readCommands(tl) (newList) (newBindList)(false)(functListList)(nested) 
                                else 
                                    let newFunctList = callFunctCommands(strip)(stack)(bindListList)(functListList)(true) in
                                    readCommands(tl)(stack)(bindListList)(true)(newFunctList)(nested) in  
                    

        
        

    (*Contains current stack...remember to print later*)
    let stack = [] in
    let bindList = [] in
    let bindListList = [] @ [bindList] in
    let functList = [] in
    let functListList = [] @ [functList] in

    readCommands(ls_str)( stack)(bindListList)(false)(functListList)(false);;
    
    (*interpreter("input1.txt", "output1.txt");; *)