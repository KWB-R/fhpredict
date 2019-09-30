$(document).ready(function() {
// OPENCPU JS
//calls R function: for risk calculation
  $("#postbutton").click(function() {

    var req1 = ocpu.call(
      "get_radolan_urls_for_measurements", {
        user_id:5, spot_id:41, sampling_time:"1050"

      },
    	function(session) {

  		  //read the session properties (just for fun)

        $("#urls_key").text(session.getKey());
        $("#urls_location").text(session.getLoc());

        //urls.getConsole(function(text) {
        //  $("#output").text(text);
        //});

        session.getObject(function(data){
          //data is the object returned by the R function
          $("#progress").text(
            data.length + " URLs retrieved.\nDownloading the files will take " +
            "about " + data.length * 1.5 / 60 + " minutes."
          );
        });


      		/*var req2 = ocpu.call("simulate_risk", {config: session1}, function(session2) {

          //read the session properties (just for fun)
          $("#key_risk").text(session2.getKey());
          $("#location_risk").text(session2.getLoc());
          //retrieve session console (async)
        	session2.getConsole(function(outtxt) {
          $("#output").text(outtxt);});
        	$("#plotdiv").rplot("plot_inflow",
          {risk: session2})
      		});
    			*/
    	}
    );

    $("#progress").text("Determining URLs to required Radolan files...");
  });
});
