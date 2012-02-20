<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <title>Hierarchiche Projektstruktur des Projekts</title>
 </head>
 <body>

<?php

$pro_dir = "./";
$depth=0;
$haddock_path = "../Haddock_Documentation";
$latex_path = "../LaTeX_Documentation";
$project_name = "unbekannt";

if (!empty($argc)) {
$pro_dir = $argv[1];
}
if ($argv>1){
$project_name=$argv[2];
}

?>

<h2>Ordnerstruktur des Projekts  <?php echo $project_name ?></h2>
<table frame = "box" rules= "all" width = "100%">
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
read_dir($pro_dir,$depth,$haddock_path,$latex_path);
?>
</tbody>
</table>

<?php
	function read_dir($dir,$depth,$haddock_path,$latex_path) {
	    $tabulator = "";
	    $cd_string = "";
	    for ($i = 0; $i < $depth; $i++) {
    		$tabulator = $tabulator."&nbsp;"."&nbsp;";
		$cd_string = "${cd_string}../";
	    }
	    $path = opendir($dir);
	    $already_done = array();
	    $text =  readdir($path);
	    $lhs_file = false;
	    $code_seq = false;
	    while (false !== ($file = readdir($path))) {
		if($file!="." && $file!=".." & $file !=".git" && $file!="dist") {
			if(is_file($dir."/".$file)){
				$module_field = explode(".",$file);
				$module = $module_field[0];
				if ((strpos($file,".hs") != null || strpos($file,".lhs")!=null)&& !in_array($module,$already_done)){
					if (strpos($file,".lhs")!=null) {
						$lhs_file = true;
					}
					$already_done[] = $module;
					$openfile = fopen($dir."/".$file,"r");
					if (!feof($openfile)){
						$author = fgets($openfile);
						$author_field = explode(":", $author);
						if (strpos($author_field[0], "Autor")!=null){
							$author = "unbekannt";
						}
						else {
							$author = $author_field[1];
						}
						$state = fgets($openfile);
						$state_field = explode(":", $state);
						if (strpos($state_field[0] ,"Status"!=null){
							$state = "unbekannt";
						}
						else {
							$state = $state_field[1];
						}
						while(!feof($openfile)){
							$moduleName = fgets($openfile);
							$spos_code_beg = strpos($moduleName,"\begin{code}");
                                                        $spos_code_end = strpos($moduleName,"\end{code}");
							if(is_int($spos_code_beg)){
								$code_seq = true;
							}
                                                        if(is_int($spos_code_end)){
                                                                $code_seq = false;
                                                        }
							$spos_mod = strpos($moduleName,"module");
							if(is_int($spos_mod)){
								$module_trim = trim($moduleName);
								$module_field = explode(" ",$module_trim);
								$spos_mod2 = strpos($module_field[0],"--");
								if(!(is_int($spos_mod2))){
									if($lhs_file == false || ($code_seq == true)){
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
					global $pro_dir;
					$h_path = "${pro_dir}/${haddock_path}/${moduleName}.html";
					$l_path = "${pro_dir}/${latex_path}/${latexfile}";
					//if(!fopen("$dir/${cd_string}${haddock_path}/${moduleName}.html","r")) {$h_path = "";}
					//if(!fopen("$dir/${cd_string}${latex_path}/${latexfile}","r")) {$l_path = "";}
					$dc = "\"";
?>
					<tr bgcolor="#FFFFCC">
						<td> <?php echo $tabulator.$file ?>  </td>
						<td> <?php echo $author ?> </td>
						<td> <?php echo $state ?> </td>
						<td> <?php 
						     if(fopen("$dir/${cd_string}${haddock_path}/${moduleName}.html","r")) { ?>
							<a href=<?php global $pro_dir; echo  "'${h_path}'" ?> >Haddock</a> 
							<?php 
						     } ?> 
						</td>
						<td> <?php
							if(fopen("$dir/${cd_string}${latex_path}/${latexfile}","r")) { ?>
								<a href=<?php echo "'${l_path} '"?> > Latex</a> </td>
								<?php
							} ?>
						<td> <?php echo date ("F d Y H:i:s.", filemtime($dir."/".$file)) ?></td>
					</tr>  
 
<?php				}
			}
			else {
?>
			<tr>
				<td> <?php echo $tabulator.$file ?> </td>
				<td> </td>
				<td> </td>
				<td> </td>
				<td> </td>
				<td> <?php echo date ("F d Y H:i:s.", filemtime($dir."/".$file)) ?> </td>

			</tr>
 
<?php	                read_dir($dir."/".$file,$depth+1,$haddock_path,$latex_path);			
			}
		}
	    }
	}
?>

 </body>
</html>

