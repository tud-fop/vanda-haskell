<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<?php
	$project_name = "Vanda";
	//$pro_dir = "/home/student/lindal/git/vanda/";
	$pro_dir = "/home/gdp/buechse/workspace/vanda/vanda/";
	$source_path = $pro_dir."src/";
	$haddock_path = $pro_dir."dist/doc/html/".$project_name."/";
	$latex_path = $pro_dir."dist/doc/latex/";
	$target_hPath = "html/"; // beschreibt den relativen Pfad der Haddock-Dokumentation bezüglich der entstehenden HTML-Datei
	$target_lPath = "latex/"; // beschreibt den relativen Pfad der LaTeX-Dokumentation bezüglich der entstehenden HTML-Datei
?>
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
		<title>Modulstruktur von <?php echo $project_name ?></title>
	</head>
	<body>
		<!--h2>Modulstruktur von <?php echo $project_name ?></h2-->
		<table frame="box" rules="all" width="100%">
			<colgroup>
				<col width="3*">
				<col width="1*">
				<col width="1*">
				<col width="1*">
				<col width="1*">
				<col width="1*">
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
	if ($argc > 0) {
		$pro_dir = $argv[1];
	}
	if ($argc > 1) {
		$project_name = $argv[2];
	}

	read_dir("","",$haddock_path,$latex_path);
?>
			</tbody>
		</table>
<?php
	function read_dir($reldir, $tabulator) {
		global $source_path;
		global $haddock_path;
		global $latex_path;
		global $target_hPath;
		global $target_lPath;
		$module_path = str_replace("/", "-", $reldir);
		$dir = $source_path.$reldir;
		$path = opendir($dir);
		while ($file = readdir($path)) {
			if ($file != "." && $file != "..") {
				if (is_file($dir.$file)) {
					$field = explode(".", $file);
					if ($field[1] == "hs" || $field[1] == "lhs") {
						$moduleName = $module_path.$field[0];
						if (false !== ($openfile = fopen($dir.$file, "r"))) {
							$author = "(nicht gesetzt)";
							$state = "(nicht gesetzt)";
							$i = 20;
							while (!feof($openfile) && $i > 0){
								$line = fgets($openfile);
								$field = explode(":", $line);
								if (sizeof($field) > 1) {
									if (strpos($field[0], "Maintainer") != null) {
										$author = trim($field[1]);
									}
									else if (strpos($field[0], "Stability") != null) {
										$state = trim($field[1]);
									}
								}
								$i = $i - 1;
							}
						}
						$h_path = "${target_hPath}${moduleName}.html";
						$l_path = "${target_lPath}${moduleName}.pdf";
?>
				<tr bgcolor="#FFFFCC">
					<td><?php echo $tabulator.$file ?></td>
					<td><?php echo $author ?></td>
					<td><?php echo $state ?></td>
					<td><?php if (fopen("${haddock_path}${moduleName}.html", "r")) { ?><a href=<?php echo "'${h_path}'" ?>>Haddock</a><?php } ?></td>
					<td><?php if (fopen("${latex_path}${moduleName}.pdf", "r")) { ?><a href=<?php echo "'${l_path} '"?>> Latex</a><?php	} ?></td>
					<td> <?php echo date ("Y-m-d H:i:s", filemtime($dir."/".$file)) ?></td>
				</tr>
<?php
					}
				}
				else if (is_dir($dir.$file)) {
?>
				<tr>
					<td colspan="5"><?php echo $tabulator.$file ?></td>
					<td><?php echo date ("Y-m-d H:i:s", filemtime($dir."/".$file)) ?></td>
				</tr>
<?php
					read_dir($reldir.$file."/",$tabulator."&nbsp;"."&nbsp;");			
				}
			}
		}
	}
?>
	</body>
</html>
