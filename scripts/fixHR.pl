# /usr/bin/perl -w
%stateAbbr = (
    "Kentucky" => "KY",
    "Virginia" => "VA",
    "Tennessee" => "TN",
    "Pennsylvania" => "PA",
    "Louisiana" => "LA",
    "Iowa" => "IA",
    "Georgia" => "GA",
    "Arkansas" => "AK",
    "Hawaii" => "HI",
    "Kansas" => "KS",
    "Vermont" => "VT",
    "West Virginia" => "WV",
    "North Dakota" => "ND",
    "South Dakota" => "SD",
    "New York" => "NY",
    "Alaska" => "AK",
    "Connecticut" => "CT",
    "Maine" => "ME",
    "Minnesota" => "MN",
    "Nevada" => "NV"
    );

open (HR,"resultsHouse2018.txt");

while ($line = <HR>) {
#    print $line;
    if ($line =~ /^([A-Z][a-z]+)\s+District\s+([1-9]\d*)$/) {
	if ($stateAbbr{$1}) { $abbr = $stateAbbr{$1} }
	else { $abbr = $1; $abbr =~ s/^([A-Z][a-z]).*/$1/; }
	$state_district = uc($abbr).",".$2; 
#	print uc($abbr), ",", $2; 
    }
    elsif ($line =~ /^([A-Z][a-z]+)\s([A-Z][a-z]+)\s+District\s+([1-9]\d*)$/) {
	$state_district = substr($1,0,1).substr($2,0,1).",".$3
#	print substr($1,0,1),substr($2,0,1),",",$3
    }
    elsif ($line =~ /^([^\d]*)\s+(\d\d?\.\d)/) {
	$str = $line;
	while ($str =~ /^([^\d]*)\s+(\d\d?\.\d)\s*/) {
	    print $state_district,",",$1,",",$2,"\n";
	    $str = $';
	}
    }	
#    else { print $line, "\n"; }
}
    
