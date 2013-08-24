function functionTwo(alertText) {
    alert(alertText);
}

window.onload = function()
{
    window.iteration = document.getElementById('poem')
                               .getAttribute('iteration');
    window.batchsize = 20;

    alert(iteration);
    alert(batchsize);

    req = new XMLHttpRequest();
    req.open('GET','updates/' + iteration + '/' + batchsize); 
    req.onreadystatechange = function(){
           if(req.readyState == 4){
               functionTwo(req.responseText);       
           }
        }
    req.send();
};

