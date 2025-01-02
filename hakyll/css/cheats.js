
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
var fileextension = [".gif", ".png", ".jpg",".JPG", ".jpeg", ".bmp"];

function stripLeadingStuff(path) {
	// Use a regular expression to match leading ../ or ../../ patterns
	return path.replace(/^((\.\.\/)+)/, '');
}

function AutoGallery(dir){
	$.ajax({
	    //This will retrieve the contents of the folder if the folder is configured as 'browsable'
	    url: dir,
	    success: function (data) {
	        //List all .png file names in the page
	        fileextension.forEach(function(e){
        		$(data).find("a:contains(" + e + ")").each(function () {
		            var filename = this.href.replace(/^.*[\\\/]/, '')
					filename = decodeURIComponent(filename); // Decode any already-encoded characters
					var image = encodeURIComponent("../" + stripLeadingStuff(dir) + "/" + filename)
					console.log("../" + stripLeadingStuff(dir) + "/" + filename)
		            //$('#gallery').append("<img src='" + dir + "/" + filename + "'>");
					$('#gallery').append("<img src='https://pdlla.org/php/resizeimage.php?maxWidth=500&imagePath=" + image + "'>");
        		})
        	});
	    }
	});
}
