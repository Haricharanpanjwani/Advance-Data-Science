<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<!DOCTYPE html>
  <html>
      <head>
        <title>San Francisco Crime</title>
        <script type="text/javascript">
        function isNumber(evt) {
            evt = (evt) ? evt : window.event;
            var charCode = (evt.which) ? evt.which : evt.keyCode;
            if (charCode > 31 && (charCode < 48 || charCode > 57)) {
            	alert("You can enter only digits!")
                return false;
            }
            return true;
        }
        </script>
        
          <script type="text/javascript">
            //<![CDATA[
            try{if (!window.CloudFlare) {var CloudFlare=[{verbose:0,p:0,byc:0,owlid:"cf",bag2:1,mirage2:0,oracle:0,paths:{cloudflare:"/cdn-cgi/nexp/dok3v=1613a3a185/"},atok:"b274a8634124cafa9b77ae2d5fbebca3",petok:"0c4931b11db65e057eed90a30d0139b933bb7b0a-1470431826-1800",zone:"easy-development.com",rocket:"0",apps:{"abetterbrowser":{"ie":"7"}},sha2test:0}];!function(a,b){a=document.createElement("script"),b=document.getElementsByTagName("script")[0],a.async=!0,a.src="//ajax.cloudflare.com/cdn-cgi/nexp/dok3v=0489c402f5/cloudflare.min.js",b.parentNode.insertBefore(a,b)}()}}catch(e){};
            //]]>
            </script>
            <script src="resources/uff/jquery.js"></script>
            <link href="resources/uff/animate.css" rel="stylesheet" type="text/css">
            <script src="resources/uff/jquery-ultimate-fancy-form.min.js"></script>
            <link href="resources/uff/jquery-ultimate-fancy-form.css" rel="stylesheet" type="text/css">
            <link href="resources/assets/bootstrap/css/bootstrap.min.css" rel="stylesheet" type="text/css">
            <script src="resources/assets/bootstrap/js/bootstrap.min.js"></script>
            <script src="resources/assets/bootstrap/js/bootstrap.js"></script>
            <link href="resources/assets/bootstrap/css/bootstrap.css" rel="stylesheet" type="text/css">
            <link href="resources/assets/registration/custom.css" rel="stylesheet" type="text/css">            
      </head>    
    <body>    
    	<c:if test="${not empty error}">
    		<div class="form-group row">        	    	
		    	<div class="alert alert-danger">    		
		    		<c:if test="${error == 'no_value'}">
		  				One or more field is empty. Please enter the value!
		  			</c:if> 
		  			<c:if test="${error == 'hour'}">
		  				Time can be only in between 0 and 23. Please re-enter the value!
		  			</c:if>    	
		  			<c:if test="${error == 'invalid'}">
		  				Invalid Output!
		  			</c:if>    	
		  			<c:if test="${error == 'maxdigit'}">
		  				Maximum 5 digits are allowed.
		  			</c:if>    	
		    	</div>	    		    		    	
	    	</div>
	    </c:if>
    	<div class="nav-control">   
      <div class="container">
        <div class="form-group row">
        	<div>
           		<center><h2> San Francisco Crime Analysis </h2></center>
            </div>
        </div>
        
        <div class="clearfix"></div>

		<div class="form-group row col-lg-4 col-lg-offset-3">
            
            <div class="col-sm-2">
              	<a target="_blank" href="resources/Doc.pdf"> Documentation </a>
            </div>
            <div class="col-sm-1">
              	<a target="_blank" href="https://powerbi.microsoft.com/en-us/landing/signin/?ru=https%3A%2F%2Fapp.powerbi.com%2F%3Froute%3Dgroups%252fme%252freports%252fccccaf90-f196-42b8-bfe0-f98ed7452027%252fReportSection11%26noSignUpCheck%3D1"> 
              	Power BI </a>     
            </div>
            <div class="col-sm-1">         	
              	<a target="_blank" href="https://github.com/Haricharanpanjwani/Advance-Data-Science/tree/master/Case%20Study%203"> GitHub </a>
            </div>             
            <div class="col-sm-1">         	
              	<a target="_blank" href="resources/presentation.pptx"> Presentation </a>
            </div>
            <div class="col-sm-1">         	
              	<a target="_blank" href="resources/testdata.xlsx"> Test Data </a>
            </div>                   
        </div>

        <form method="POST" action="service">          

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="region" class="col-sm-2 col-form-label">Region</label>
            <div class="col-sm-5">
              <select id="region" name="region" class="form-control">
                <option value="Bayview">Bayview</option>
                <option value="Central">Central</option>                
                <option value="Ingleside">Ingleside</option>
                <option value="Mission">Mission</option>
                <option value="Northern">Northern</option>
                <option value="Park">Park</option>
                <option value="Richmond">Richmond</option>                
                <option value="Sourthen">Southern</option>
                <option value="Taraval">Taraval</option>
                <option value="Tenderloin">Tenderloin</option>                
              </select>
            </div>
          </div>

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="day" class="col-sm-2 col-form-label">Day of Week</label>
            <div class="col-sm-5">
              <select id="day" name ="day" class="form-control">
                <option value="Monday">Monday</option>
                <option value="Tuesday">Tuesday</option>                
                <option value="Wednesday">Wednesday</option>
                <option value="Thursday">Thursday</option>
                <option value="Friday">Friday</option>
                <option value="Saturday">Saturday</option>
                <option value="Sunday">Sunday</option>                                
              </select>
            </div>
          </div>

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="date" class="col-sm-2 col-form-label">Date</label>
            <div class="col-sm-5">
              <input type="date" class="form-control" id="date" name="date" placeholder="Date">
            </div>
          </div>

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="time" class="col-sm-2 col-form-label">Time (in Hours)</label>
            <div class="col-sm-5">
              <input type="number" class="form-control" id="time" name="time" placeholder="Time">
            </div>
          </div>

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="latitude" class="col-sm-2 col-form-label">Latitude</label>
            <div class="col-sm-1">
              <input type="text"  class="form-control" id="latitude" value="-122." readonly="true">
            </div>
            <div class="col-sm-2">
              <input type="text" maxlength="6" class="form-control" id="latitude" name="latitude" placeholder="Latitude" onkeypress="return isNumber(event)">
            </div>
          </div>

          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="longitude" class="col-sm-2 col-form-label">Longitude</label>
            <div class="col-sm-1">
              <input type="text" class="form-control" id="longitude" value="37." readonly="true">
            </div>
            <div class="col-sm-2">
              <input type="text" maxlength="6" class="form-control" id="longitude" name="longitude" placeholder="Longitude" onkeypress="return isNumber(event)">
            </div>
          </div>
          
          <div class="form-group row col-lg-4 col-lg-offset-2">
            <label for="predict" class="col-sm-2 col-form-label">Choose Model</label>            
            <div class="col-sm-6">
              	<label class="radio-inline"><input type="radio" name="model" value="forest" checked="checked">Random Forest</label>
			  	<label class="radio-inline"><input type="radio" name="model" value="neural">Neural Network</label>
				<label class="radio-inline"><input type="radio" name="model" value="decision">Decision Jungle</label>
            </div>
          </div>                            
            
            <div class="form-group row">     
	            <div class="col-lg-4 col-lg-offset-4">
	              <input type="submit" class="btn btn-primary" />
	            </div>
            </div>
        </form>        
      </div>

      </div>
      
      <div class="nav-control">
      <div class="form-group row">   
      		<c:if test="${not empty output}">
      		<div class="alert alert-success">	      		
	      			<h3>Predicted crime by model is: <c:out value="${output}"></c:out></h3>
	      	</div>
      		</c:if>
      </div>
      </div>
      
  </body>
 </html>