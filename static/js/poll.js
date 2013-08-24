function processChangeBatch(changeBatch) {

//    for (var i = 0; i < changeBatch.length; i++) {
//            alert(changeBatch[i][2]);
//        }
//    alert(alertText);
    scheduleChange(changeBatch,0);
}

function scheduleChange(changeBatch,index) {
    var change = changeBatch[index];
    var delayMilis = change[0];

    setTimeout(function() { executeChange(changeBatch,index); } ,delayMilis);  
}

// The function that goes into the timer.
function executeChange(changeBatch,index) {
    var change = changeBatch[index];
    var verseIndex = change[1];
    var verseText = change[2];

    if (verseIndex == 0) {
        document.getElementById('poemtitle').innerHTML = verseText; 
    } else {
        document.getElementById('v'+verseIndex).innerHTML = verseText; 
    } 

    // alert(verseIndex);
    // alert(verseText);

    scheduleChange(changeBatch,index+1);
}

window.onload = function()
{
    window.iteration = document.getElementById('poem')
                               .getAttribute('iteration');
    window.batchsize = 20;

    req = new XMLHttpRequest();
    req.open('GET','updates/' + iteration + '/' + batchsize); 
    req.onreadystatechange = function(){
           if(req.readyState == 4){
               processChangeBatch(JSON.parse(req.responseText));       
           }
        }
    req.send();
};

