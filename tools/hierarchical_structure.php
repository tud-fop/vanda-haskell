<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
		<title>Hierarchische Projektstruktur des Projekts</title>
	</head>
	<body>
		<h2>Ordnerstruktur des Projekts  <?php echo $project_name ?></h2>
		<table frame="box" rules="all" width="100%">
		<colgroup>
			<col width = "3*">
			<col width = "1*">
			<col width = "1*">
			<col width = "1*">
			<col width = "1*">
			<col width = "1*">
		</colgroup>
		<thead>
			<tr>
				<th></th>
				<th>Verantwortlicher</th>
				<th>Status</th>
				<th>Haddock</th>
				<th>LaTeX</th>
				<th>letzte &Auml;nderung</th>
			</tr>
		</thead>
		<tbody>
<?php
	$project_name = "Vanda";
	$pro_dir = "/home/student/lindal/git/vanda/";
	$haddock_path = $pro_dir."dist/doc/html".$project_name;
	$latex_path = $pro_dir."dist/doc/latex";
	$target_hPath = "html"; // beschreibt den relativen Pfad der Haddock-Dokumentation bezüglich der entstehenden HTML-Datei
	$target_lPath = "latex"; // beschreibt den relativen Pfad der LaTeX-Dokumentation bezüglich der entstehenden HTML-Datei
	if ($argv > 0) {
		$pro_dir = $argv[1];
	}
	if ($argv > 1) {
		$project_name = $argv[2];
	}

	read_dir($pro_dir."src/","",$haddock_path,$latex_path);
?>
		</tbody>
	</table>

<?php
	function read_dir($dir,$tabulator) {
		global $pro_dir;
		global $haddock_path;
		global $latex_path;
		global $target_hPath;
		global $target_lPath;
		$path = opendir($dir);
		$already_done = array();
		$text =  readdir($path);
		$lhs_file = false;
		$code_seq = false;
		while ($file = readdir($path)) {
			//print("<tr><td>$dir$file</td></tr>");
			if ($file != "." && $file != ".." & $file != ".git" && $file != "dist") {
				if (is_file($dir.$file)) {
					$module_field = explode(".",$file);
					$module = $module_field[0];
					if ((strpos($file,".hs") != null || strpos($file,".lhs")!=null)
						&& !in_array($module,$already_done)) {
						if (strpos($file,".lhs")!=null) {
							$lhs_file = true;
						}
						$already_done[] = $module;
						if (false !== ($openfile = fopen($dir."/".$file,"r"))) {
							$author = "unbekannt";
							$state = "unbekannt";
							while (!feof($openfile)){
								$line = fgets($openfile);
								$author_line = $line;
								$author_field = explode(":", $author_line);
								//echo  $author_field[0] . "\n";
								if (strpos($author_field[0], "Maintainer") === false) {
								}
								else if (sizeof($author_field) > 1) {
									$author = $author_field[1];
									//echo "++++++++++++" .  $author_field[1] . "++++++++++++\n";
								}
								$state_line = $line;
								$state_field = explode(":", $state_line);
								if (sizeof($state_field) > 1
									&& strpos($state_field[0], "Stability")!=null) {
									$state = $state_field[1];
								}

								$moduleName = $line;
								$spos_code_beg = strpos($moduleName, "\begin{code}");
								$spos_code_end = strpos($moduleName, "\end{code}");
								if (is_int($spos_code_beg)) {
									$code_seq = true;
								}
								if (is_int($spos_code_end)) {
									$code_seq = false;
								}
								$spos_mod = strpos($moduleName,"module");
								if (is_int($spos_mod)){
									$module_trim = trim($moduleName);
									$module_field = explode(" ",$module_trim);
									$spos_mod2 = strpos($module_field[0],"--");
									if (!(is_int($spos_mod2))) {
										if ($lhs_file == false || ($code_seq == true)) {
											$moduleName = $module_field[1];
											$moduleName = str_replace(".","-",$moduleName);
											$module_field = explode("(",$moduleName);
											$moduleName = $module_field[0];
											break;
										}
									}
								}
							}
						}
						$latexfield = explode(".",$file);
						$latexfile = $latexfield[0].".pdf";
						$h_path = "${target_hPath}/${moduleName}.html";
						$l_path = "${target_lPath}/${moduleName}.pdf";
						//if(!fopen("$dir/${cd_string}${haddock_path}/${moduleName}.html","r")) {$h_path = "";}
						//if(!fopen("$dir/${cd_string}${latex_path}/${latexfile}","r")) {$l_path = "";}
						$dc = "\"";
?>
					<tr bgcolor="#FFFFCC">
						<td><?php echo $tabulator.$file ?></td>
						<td><?php echo $author ?></td>
						<td><?php echo $state ?></td>
						<td><?php if (fopen("${haddock_path}/${moduleName}.html", "r")) { ?> <a href=<?php echo "'${h_path}'" ?>>Haddock</a><?php } ?></td>
						<td><?php if (fopen("${latex_path}/${moduleName}.pdf", "r")) { ?> <a href=<?php echo "'${l_path} '"?>> Latex</a><?php	} ?></td>
						<td> <?php echo date ("Y-m-d H:i:s", filemtime($dir."/".$file)) ?></td>
					</tr>
<?php
					}
				}
				else if (is_dir($dir.$file)) {
?>
					<tr>
						<td><?php echo $tabulator.$file ?></td>
						<td></td>
						<td></td>
						<td></td>
						<td></td>
						<td><?php echo date ("Y-m-d H:i:s", filemtime($dir."/".$file)) ?></td>
					</tr>
<?php
					read_dir($dir.$file."/",$tabulator."&nbsp;"."&nbsp;");			
				}
			}
		}
	}
?>

 </body>
</html>

