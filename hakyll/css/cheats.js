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