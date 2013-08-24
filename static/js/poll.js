function functionTwo(alertText) {
    window.changebatch = JSON.parse(alertText);
    for (var i = 0; i < changebatch.length; i++) {
            alert(changebatch[i][2]);
        }
    alert(alertText);
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
               functionTwo(req.responseText);       
           }
        }
    req.send();
};

