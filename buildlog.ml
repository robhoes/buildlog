let buildlog_index = "buildlog.index"
let conf_file = "buildlog.conf"

type dict_t = {
	branch: string;
	index: (string * string) list
}

let rebuild_index manifest repo branch =
	let path = manifest ^ "/carbon/" ^ branch ^ "/daily" in
	let dir = Unix.opendir path in
	let rec read_manifest ac =
		try
			let name = Unix.readdir dir in
			match name with
			| "." | ".." -> read_manifest ac
			| build ->
				let file = open_in (path ^ "/" ^ build ^ "/manifest") in
				let rec loop () =
					let line = input_line file in
					Scanf.sscanf line "%s %s %s" (fun _ repo' hash ->
						if repo' = repo then
							hash
						else
							loop ()
					)
				in
				try
					let hash = loop () in
					close_in file;
					let ac =
						if not (List.mem_assoc hash ac) then
							(hash, build) :: ac
						else if (int_of_string (List.assoc hash ac)) > (int_of_string build) then begin
							let ac = List.remove_assoc hash ac in
							(hash, build) :: ac
						end else
							ac
					in
					read_manifest ac
				with End_of_file ->
					close_in file;
					read_manifest ac
		with End_of_file ->
			Unix.closedir dir;
			ac
	in
	let index = read_manifest [] in

	let index = List.sort (fun (_, b) (_, b') -> compare (int_of_string b) (int_of_string b')) index in
	let index, _ = List.fold_left (fun (ac, prev) (h, b) ->
		match prev with
		| None -> (h, b) :: ac, Some h
		| Some h' ->
			let cmd = Printf.sprintf "git log %s..%s --pretty=oneline" h' h in
			let input = Unix.open_process_in cmd in
			let rec loop ac =
				try
					let line = input_line input in
					let h = String.sub line 0 40 in
					loop ((h, b) :: ac)
				with End_of_file ->
					ac
			in
			let extra =
				try
					ignore (input_line input);
					loop []
				with End_of_file -> []
			in
			ignore (Unix.close_process_in input);
			(h, b) :: extra @ ac, Some h
	) ([], None) index in

	let out = open_out (branch ^ "-" ^ buildlog_index) in
	List.iter (fun (h, b) -> Printf.fprintf out "%s %s\n" h b) index;
	close_out out;

	{branch = branch; index = index}

let read_index branch =
	let file = open_in (branch ^ "-" ^ buildlog_index) in
	let rec loop ac =
		try
			Scanf.fscanf file "%s %s\n" (fun hash build ->
				loop ((hash, build) :: ac)
			)
		with End_of_file ->
			ac
	in
	let index = loop [] in
	close_in file;
	{branch = branch; index = index}

let string_of_info commit dict =
	let print =
		Printf.sprintf "First %s build: %s" dict.branch
	in
	if List.mem_assoc commit dict.index then
		print (List.assoc commit dict.index)
	else
		print "none"

let is_in_range range build =
	match range with
	| None, None -> true
	| Some b, None -> build >= b
	| None, Some b -> build <= b && build > -1
	| Some b, Some b' -> build >= b && build <= b'

let output_log dicts out commit log =
	match commit with
	| None -> ()
	| Some commit' ->
		let info = List.map (string_of_info commit') dicts in
		let output = (List.hd log) :: info @ (List.tl log) in
		List.iter (fun line -> output_string out (line ^ "\n")) output

let check_range dicts range commit =
	match commit with
	| None -> false
	| Some commit ->
		let dicts =
			match range with
			| _, _, None -> dicts
			| _, _, Some branch ->
				try [List.find (fun dict -> dict.branch = branch) dicts]
				with Not_found -> []
		in
		let builds = List.map (fun dict ->
			if List.mem_assoc commit dict.index then
				int_of_string (List.assoc commit dict.index)
			else
				-1
		) dicts in
		List.fold_left (fun a b ->
			match range with
			| x, y, _ -> is_in_range (x, y) b || a
		) false builds

let process range dicts =
	let out = Unix.open_process_out "less" in
	let rec loop log commit =
		let output () =
			if log <> [] && check_range dicts range commit then
				output_log dicts out commit (List.rev log)
		in
		try
			let line = read_line () in
			Scanf.sscanf line "%s %s" (fun a b ->
				if a = "commit" then begin
					output ();
					loop [line] (Some b)
				end else
					loop (line :: log) commit
			)
		with End_of_file ->
			output ();
			Unix.close_process_out out
	in
	loop [] None

let split c s =
	let rec loop ac s' =
		try
			let i = String.index s' c in
			let a = String.sub s' 0 i in
			let s' = String.sub s' (i + 1) (String.length s' - i - 1) in
			loop (a :: ac) s'
		with Not_found ->
			s' :: ac
	in
	let words = loop [] s in
	List.rev words

let read_conf () =
	try
		let file = open_in conf_file in
		let rec read ac =
			try
				let line = input_line file in
				let words = split ' ' line in
				read ((List.hd words, List.tl words) :: ac)
			with End_of_file -> ac
		in
		let conf = read [] in
		close_in file ;
		conf
	with Sys_error _ -> []

let write_conf conf =
	let file = open_out conf_file in
	List.iter (fun (key, values) ->
		output_string file (key ^ " " ^ (String.concat " " values) ^ "\n")
	) conf;
	close_out file

let make_range range =
	try
		let branch =
			try
				let i = String.index range '@' in
				Some (String.sub range (i + 1) (String.length range - i - 1))
			with Not_found -> None
		in
		Scanf.sscanf range "%[0-9]..%[0-9]" (fun a b ->
			(try Some (int_of_string a) with _ -> None),
			(try Some (int_of_string b) with _ -> None),
			branch
		)
	with End_of_file -> None, None, None

let _ =
	(* Parse command-line arguments *)
	let rebuild = ref false in
	let manifest = ref "" in
	let repo = ref "" in
	let branches = ref "" in
	let range = ref "" in
	Arg.parse [
			"-rebuild", Arg.Set rebuild, "Rebuild the index";
			"-manifest", Arg.Set_string manifest, "Path to manifests.hg";
			"-repo", Arg.Set_string repo, "Name of the source repository";
			"-branches", Arg.Set_string branches, "List of branches";
			"-range", Arg.Set_string range, "Only show commits that entered the build in the given range (inclusive)";
		]
		(fun x -> Printf.printf "Ignoring argument: %s" x)
		"Utility that links build numbers and repository logs";

	let conf = read_conf () in
	if conf = [] && not !rebuild
	then begin
		print_endline "No config file found. Please run the command with the -rebuild option:" ;
		print_string  "git log | buildlog -rebuild -manifest <path_to_manifests.hg> " ;
		print_endline "-repo <xen-api.git|xen-api-libs.git> -branches trunk,boston" ;
		exit 1
	end ;
	let manifest, conf =
		if !manifest <> "" then begin
			ignore (List.remove_assoc "manifest" conf);
			!manifest, ("manifest", [!manifest]) :: conf
		end else
			List.hd (List.assoc "manifest" conf), conf
	in
	let repo, conf =
		if !repo <> "" then begin
			ignore (List.remove_assoc "repo" conf);
			!repo, ("repo", [!repo]) :: conf
		end else
			List.hd (List.assoc "repo" conf), conf
	in
	let branches, conf =
		if !branches <> "" then begin
			ignore (List.remove_assoc "branches" conf);
			let branches' = split ',' !branches in
			branches', ("branches", branches') :: conf
		end else
			List.assoc "branches" conf, conf
	in
	write_conf conf;
	let range' = make_range !range in

	let dicts = List.map (fun branch ->
			if !rebuild then
				rebuild_index manifest repo branch
			else
				read_index branch
		) branches
	in
	process range' dicts

