package com.azure.api;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Iterator;
import java.util.zip.InflaterInputStream;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

public class AzureML {
	   
	    public static String apiurl;
	    public static String apikey;
	    
	    // Calling random Forest model
	    public String callDecisionForestService(String json) {
	    	System.out.println("calling random forest model: ");
	    	
	    	apiurl = "https://ussouthcentral.services.azureml.net/workspaces/4b54602ea43446c29c5950caa5563f70/services/38d57a81894043c69691250a23118150/execute?api-version=2.0&details=true";
	    	apikey = "/hJikrmfUMHJZIZe/lUqGgfqHV7SAdBgNX1ENcowoKZemM6TyTtsSXQlhhvSZWfeHbhFP5+v8DFHtyCLnKgtRg==";
	    		    	
	    	String result = rrsHttpPost(json);
	    	return retrieveOutput(result);
	    }
	    
	    // Calling Neural network model
	    public String callNeuralNetworkService(String json) {
	    	System.out.println("calling neural network model: ");
	    	
	    	apiurl = "https://ussouthcentral.services.azureml.net/workspaces/4b54602ea43446c29c5950caa5563f70/services/07255bc333704865bd65e9deb975c754/execute?api-version=2.0&details=true";
	    	apikey = "R6Y9SnqjQf2WMg3/oL/Xex4KoD7uW9bhu21XKYcMBQ+DpdVbafcXBwaatl9cYNohvsv1Q4GJNQlB9hHgOh1JVQ==";
	    	
	    	String input = rrsHttpPost(json);
	    	return retrieveOutput(input);
	    }
	    
	    // Calling Decision Jungle model 
	    public String callDecisionJungleService(String json) {
	    	System.out.println("calling decision jungle model: ");
	    	
	    	apiurl = "https://ussouthcentral.services.azureml.net/workspaces/4b54602ea43446c29c5950caa5563f70/services/c82d69e8862f49a68c85034c2f8b7889/execute?api-version=2.0&details=true";
	    	apikey = "8eH3PJdWKnnRU0KPq/NANY4fgM7nAYlVnaAlZ+nD2+d8JH6lAOQmOaHJ/5hVw87eW8o/7hbYCz5UJBeg0J6tIg==";
	    	
	    	String input = rrsHttpPost(json);
	    	return retrieveOutput(input);	
	    }
	    
	   
	    /**
	     * Call REST API for retrieving prediction from Azure ML 
	     * @return response from the REST API
	     */	
	    public String rrsHttpPost(String jsonBody) {
	        
	        HttpPost post;
	        HttpClient client;
	        StringEntity entity;
	        
	        try {
	        	
	            // create HttpPost and HttpClient object
	            post = new HttpPost(apiurl);
	            client = HttpClientBuilder.create().build();
	            
	            // setup output message by copying JSON body into 
	            // apache StringEntity object along with content type
	            entity = new StringEntity(jsonBody, HTTP.UTF_8);
	            entity.setContentEncoding(HTTP.UTF_8);
	            entity.setContentType("text/json");

	            // add HTTP headers
	            post.setHeader("Accept", "text/json");
	            post.setHeader("Accept-Charset", "UTF-8");
	        
	            // set Authorization header based on the API key
	            post.setHeader("Authorization", ("Bearer "+apikey));
	            post.setEntity(entity);

	            // Call REST API and retrieve response content
	            HttpResponse authResponse = client.execute(post);
	            
	            return EntityUtils.toString(authResponse.getEntity());
	            
	        }
	        catch (Exception e) {   
	            System.out.println("Error occurred while calling the service!!");
	            return e.toString();
	        }
	    }	 
	    
	    public String retrieveOutput(String input) {
			
	    	try {
		    	JSONParser parser = new JSONParser();	    	
		    	Object obj = parser.parse(input);	 
		    	
				JSONObject json = (JSONObject) obj;
								
			   JSONObject result = (JSONObject) json.get("Results");
			   System.out.println(json.get("Results"));
				
		       JSONObject output1 = (JSONObject)result.get("output1");
		       System.out.println(result.get("output1"));	
		       
		       JSONObject value = (JSONObject)output1.get("value");
		       System.out.println(output1.get("value"));
		        
		       String res = null; 
		       JSONArray strArray = (JSONArray)value.get("Values");				
		       Iterator<Object> itr = strArray.iterator();
			
				
				while(itr.hasNext()) {
					res = itr.next().toString();
					break;
				}
				
				StringBuilder sb = new StringBuilder();
				sb.append(res);
				
				String s = res.substring(2,sb.indexOf("]"));				
				
				String output =	s.substring(0,s.lastIndexOf('"'));
			 
				System.out.println(output);
				return output;
	    	}
	    	catch(Exception e) {
	    		System.out.println("Error while parsing output!!");
	    		e.printStackTrace();;
	    	}
	    	
	    	return null;
	    }
	    }