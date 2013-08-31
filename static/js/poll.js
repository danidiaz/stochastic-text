function requestChangeBatch(baseIndex, batchSize) {
    
    if ($debug) {
        console.log( 'Request change batch.'+
                     ' baseIndex=' + baseIndex +' batchSize='+batchSize);
    }

    var req = new XMLHttpRequest();
    req.open('GET','updates/' + baseIndex + '/' + batchSize); 
    req.onreadystatechange = function(){
           if(req.readyState == 4){
               scheduleChange(baseIndex, 0, JSON.parse(req.responseText));       
           }
        }
    req.send();
}

function scheduleChange(baseIndex,index,changeBatch) {
    var change = changeBatch[index];
    var delayMilis = change[0];

    setTimeout(function() { executeChange(baseIndex, index, changeBatch); }, delayMilis);  
}

function executeChange(baseIndex,index,changeBatch) {
    var change = changeBatch[index];
    var verseIndex = change[1];
    var verseText = change[2];

    if ($debug) {
        var iteration = change[3];
        console.log( 'Executing change. ' + 
                     ' verse_index=' + verseIndex + 
                     ' index=' + iteration +
                     ' text=' + verseText
                   );
    }

    if (verseIndex == 0) {
        document.getElementById('poemtitle').innerHTML = verseText; 
    } else {
        var target = document.getElementById('V'+verseIndex); 

        if ($debug) {
            if (target.innerHTML.trim() == verseText) {
                console.log('DUPLICATED VERSE!');
            }
        }

        target.innerHTML = verseText; 
    } 

    var nextIndex = index + 1;
    var batchSize = changeBatch.length;
    if (nextIndex == batchSize) {
        requestChangeBatch(baseIndex + batchSize, batchSize);
    } else {
        scheduleChange(baseIndex,nextIndex,changeBatch);
    } 
}

var $debug = true

window.onload = function()
{
    var iteration = parseInt ( document.getElementById('poem')
                                       .getAttribute('iteration') );
    requestChangeBatch(iteration,20);
};

