package com.azure.api;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

/**
 * Handles requests for the application home page.
 */
@Controller
public class HomeController {
	
	final static String apikey = "GeG32HHijMlWdeIa/JcaV7QKFbpoezpaNXHib3+mEep+1xAcyO1vlwSC9GOatsmxfyrt+prrA55+1vbjkyah9g==";
    final static String apiurl = "https://ussouthcentral.services.azureml.net/workspaces/253767f4788549c1baaad7723c456e77/services/27e620cbde5548348d16c1ae1653e560/execute?api-version=2.0&details=true";
    public static String jsonBody;

	private static final Logger logger = LoggerFactory.getLogger(HomeController.class);

	/**
	 * Simply selects the home view to render by returning its name.
	 */

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public String home(Locale locale, Model model) {
		return "index";
	}
	
	@RequestMapping(value = "/service", method = RequestMethod.POST)
	public ModelAndView service(HttpServletRequest request, HttpServletResponse response) {		
		
		ModelAndView mv= new ModelAndView();
		mv.setViewName("index"); 
		
		String district = request.getParameter("region");
		String dayText = request.getParameter("day");
		String date = request.getParameter("date");
		String time = request.getParameter("time");
		String latitude = request.getParameter("latitude");
		String longitude = request.getParameter("longitude");
		String model = request.getParameter("model");
		
		int weekday = -1;
		int month = -1;
		int day = -1;
		String jsonBody = null;
		
		if(district == "" || date == "" || time == "" || 
			dayText == "" || latitude == "" || longitude == "") {
				mv.addObject("error", "no_value");
				return mv;
		}
		
		int hours = Integer.parseInt(time);
		System.out.println(hours);			
		
		if(hours < 0 || hours > 23) {
			mv.addObject("error", "hour");
			return mv;
		}
		
		String lat = "-122." + latitude;
		double x = Double.parseDouble(lat);
		
		String lon = "37." + longitude;		
		double y = Double.parseDouble(lon);
		
		//System.out.println(x + " " + y);	
		
		if(dayText.equals("Monday") || dayText.equals("Tuesday") ||
			dayText.equals("Wednesday") ||  dayText.equals("Thursday")) {				
				
				weekday = 0;
		}
		else	weekday = 1;
			
		
		try {
			DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd", Locale.US); 
			Date startDate = (Date)formatter.parse(date); 
			
			 Calendar cal = Calendar.getInstance();
			 cal.setTime(startDate);			 
			 month = cal.get(Calendar.MONTH) + 1;
			 day = cal.get(Calendar.DAY_OF_MONTH);		     
					

			/*creating json from form values */
			
			JSONObject obj = new JSONObject();
			JSONObject inputs = new JSONObject();
			JSONObject input = new JSONObject();
			
			JSONArray columnName = new JSONArray();
			columnName.add("PdDistrict");
			columnName.add("X");
			columnName.add("Y");
			columnName.add("Date");
			columnName.add("months");
			columnName.add("Hours");
			columnName.add("DayOfWeekMap");
			
			JSONArray allValues = new JSONArray();
			JSONArray value = new JSONArray();
			value.add(district);
			value.add(x);
			value.add(y);
			value.add(day);
			value.add(month);
			value.add(hours);
			value.add(weekday);
			allValues.add(value);
	
					
			input.put("ColumnNames", columnName);
			input.put("Values", allValues);
			inputs.put("input1", input);
			obj.put("Inputs", inputs);
	
			System.out.println("print the value of json "+obj);
			
			//converting json to string
			jsonBody = obj.toString(); 
		}
		catch(Exception e) {
			System.out.println("Error occurred!!" + e);
		}		
		
		//Creating instance of azure class
		AzureML am = new AzureML();		
		
		String res = null;
		System.out.println("Model: " + model);
		
		if(model.equals("forest")) {
			res = am.callDecisionForestService(jsonBody);
		}
		else if(model.equals("neural")) {
			res = am.callNeuralNetworkService(jsonBody);
		}
		else if(model.equals("decision")) {
			res = am.callDecisionJungleService(jsonBody);
		}
		
		if(res != null) {
			System.out.println("Result of web service :" + res);	
			
			String desc = "";
			
			if(Integer.parseInt(res) == 1) {
				desc = "LARCENY/THEFT";
			}
			else if(Integer.parseInt(res) == 2) {
				desc = "OTHER OFFENSES";
			}
			else if(Integer.parseInt(res) == 3) {
				desc = "NON-CRIMINAL";
			}
			else if(Integer.parseInt(res) == 4) {
				desc = "ASSAULT";
			}
			else if(Integer.parseInt(res) == 5) {
				desc = "DRUG/NARCOTIC";
			}
			else if(Integer.parseInt(res) == 6) 	desc = "VEHICLE THEFT";
			else if(Integer.parseInt(res) == 7)  	desc = "VANDALISM";
			else if(Integer.parseInt(res) == 8)  	desc = "WARRANTS";				
			else if(Integer.parseInt(res) == 9)	    desc = "BURGLARY";
			else if(Integer.parseInt(res) == 10)	desc = "SUSPICIOUS Occurence";
			else if(Integer.parseInt(res) == 11)	desc = "Missing Person";
			else if(Integer.parseInt(res) == 12)	desc = "Robbery";
			else  if(Integer.parseInt(res) == 13)	desc = "Fraud";
			else if(Integer.parseInt(res) == 14)	desc = "FORGERY/COUNTERFEITING";
			else if(Integer.parseInt(res) == 15)	desc = "SECONDARY CODES";
			else if(Integer.parseInt(res) == 16)	desc = "WEAPON LAWS";
			else if(Integer.parseInt(res) == 17)	desc = "PROSTITUTION";
			else if(Integer.parseInt(res) == 18)	desc = "TRESPASS";
			else if(Integer.parseInt(res) == 19)	desc = "STOLEN PROPERTY";
			
			mv.addObject("output",desc);
		}
		else {
			System.out.println("Error thrown by web service call!!");			
			mv.addObject("error","invalid");	
			return mv;
		}
						
		return mv;
	}
	
}
