$( document ).ready(function() {
 
});

$(document).ready(function() {

    
    $('#menu').click(function(e) {
      console.log("clicked");
      
      $(".sidenav").toggleClass("sidenav-opened");
      $(".main").toggleClass("main-opened");
      $(".title2").toggleClass("title2-opened");
      //$('.grid-container').css({ "grid-template-columns" : "50% 50%", "transition": "all 1s" });
      //$('.sidenav').css({ "width": "100%", "transition": "all 1s" })
    });

  });
