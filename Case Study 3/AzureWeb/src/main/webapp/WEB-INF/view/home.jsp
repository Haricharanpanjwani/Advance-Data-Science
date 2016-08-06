<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<!DOCTYPE html>
  <html>
      <head>
        <title>San Francisco Crime</title>
      
          <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">

          <!-- Optional theme -->
          <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">

          <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"></script>

          <!-- Latest compiled and minified JavaScript -->
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>

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
    	   
      <div class="container">
        <div class="form-group row">
            <h2> San Francisco Crime Analysis </h2>
         </div>
      
        <form method="POST" action="service">          

          <div class="form-group row">
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

          <div class="form-group row">
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

          <div class="form-group row">
            <label for="date" class="col-sm-2 col-form-label">Date</label>
            <div class="col-sm-5">
              <input type="date" class="form-control" id="date" name="date" placeholder="Date">
            </div>
          </div>

          <div class="form-group row">
            <label for="time" class="col-sm-2 col-form-label">Time (in Hours)</label>
            <div class="col-sm-5">
              <input type="number" class="form-control" id="time" name="time" placeholder="Time">
            </div>
          </div>

          <div class="form-group row">
            <label for="latitude" class="col-sm-2 col-form-label">Latitude</label>
            <div class="col-sm-1">
              <input type="text"  class="form-control" id="latitude" value="-122." readonly="true">
            </div>
            <div class="col-sm-2">
              <input type="number" max = 1000000 class="form-control" id="latitude" name="latitude" placeholder="Latitude">
            </div>
          </div>

          <div class="form-group row">
            <label for="longitude" class="col-sm-2 col-form-label">Longitude</label>
            <div class="col-sm-1">
              <input type="text" class="form-control" id="longitude" value="37." readonly="true">
            </div>
            <div class="col-sm-2">
              <input type="number" max=1000000 class="form-control" id="longitude" name="longitude" placeholder="Longitude">
            </div>
          </div>
          
          <div class="form-group row">
            <label for="predict" class="col-sm-2 col-form-label">Choose Model</label>            
            <div class="col-sm-2">
              <label class="radio-inline"><input type="radio" name="model" value="forest" checked="checked">Random Forest</label>
			  <label class="radio-inline"><input type="radio" name="model" value="neural">Neural Network</label>
			  <label class="radio-inline"><input type="radio" name="model" value="decision">Decision Jungle</label>
            </div>
          </div>
                    
                  
          <div class="form-group row" align="center">
            <div class="offset-sm-2 col-sm-10">
              <input type="submit" class="btn btn-primary" />
            </div>
          </div>
        </form>        
      </div>
      
      <div class="form-group row">   
      		<c:if test="${not empty output}">
      		<div class="alert alert-success">
	      		<div class="col-sm-6">
	      			<h3>Predicted crime by model is: <c:out value="${output}"></c:out></h3>
	      		</div>
	      	</div>
      		</c:if>
      </div>
      
  </body>
 </html>