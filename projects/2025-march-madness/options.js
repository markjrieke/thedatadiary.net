var dropDown = document.getElementById("tourneyState");

// initially display the first element
document.getElementById(ids[0]).style.display="block";
for (var x = 1; x < ids.length; x++) {
  document.getElementById(ids[x]).style.display="none";
}

// update display when elements are selected
dropDown.onchange = function(){
  for(var x = 0; x < ids.length; x++){   
    document.getElementById(ids[x]).style.display="none";
  }    
  document.getElementById(this.value).style.display = "block";
}