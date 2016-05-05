structure Prelude =
struct
  infixr 0 $
  fun f $ x = f x

  fun println str = print (str ^ "\n")

  (* Vector helpers *)
  fun vectorToList arr = Vector.foldr (op ::) [] arr

  (* Main helpers *)
  fun makeMain main () =
    let val args = CommandLine.arguments () in
      OS.Process.exit (main args; OS.Process.success)
    end
end
