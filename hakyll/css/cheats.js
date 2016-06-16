
//jquery function to randomize order of elements
(function($) {
$.fn.randomize = function(childElem) {
  return this.each(function() {
      var $this = $(this);
      var elems = $this.children(childElem);

      elems.sort(function() { return (Math.round(Math.random())-0.5); });  

      $this.detach(childElem);  

      for(var i=0; i < elems.length; i++)
        $this.append(elems[i]);      

  });    
}
})(jQuery);

//used to pull gallery images using dreamhost generated directory html
var fileextension = [".png", ".jpg",".JPG", ".gif"];

function AutoGallery(dir){
	$.ajax({
	    //This will retrieve the contents of the folder if the folder is configured as 'browsable'
	    url: dir,
	    success: function (data) {
	        //List all .png file names in the page
	        fileextension.forEach(function(e){
        		$(data).find("a:contains(" + e + ")").each(function () {
		            var filename = this.href.replace(/^.*[\\\/]/, '')
		            $('#gallery').append("<img src='" + dir + "/" + filename + "'>");
        		})
        	});
	    }
	});
}
