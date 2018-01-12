/*******************************************************************************
 * Copyright 2018 Observational Health Data Sciences and Informatics
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package org.ohdsi.sql;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the command line interface. The command line interface is for non-Java non-R tool developers that still want to use SqlRender.
 */
public class MainClass {

	public static void main(String[] args) {
		if (args.length < 4 || (args.length > 0
				&& (args[0].toLowerCase().equals("-usage") || args[0].toLowerCase().equals("-help") || args[0].toLowerCase().equals("?")))) {
			printUsage();
			return;
		}
		String sql = readFile(args[0]);

		// Render
		for (int i = 2; i < args.length; i++)
			if (args[i].equals("-render")) {
				List<String> parameters = new ArrayList<String>();
				List<String> values = new ArrayList<String>();
				for (int j = i + 1; j < args.length - 1; j += 2) {
					if (args[j].equals("-translate") || args[j].equals("-oracle_temp_schema") || args[j].equals("-session_id"))
						break;
					parameters.add(args[j]);
					values.add(args[j + 1]);
				}
				sql = SqlRender.renderSql(sql, parameters.toArray(new String[parameters.size()]), values.toArray(new String[parameters.size()]));
			}

		// Translate
		String oracleTempSchema = null;
		for (int i = 2; i < args.length - 1; i++)
			if (args[i].equals("-oracle_temp_schema")) {
				oracleTempSchema = args[i + 1];
				break;
			}
		String sessionId = null;
		for (int i = 2; i < args.length - 1; i++)
			if (args[i].equals("-session_id")) {
				sessionId = args[i + 1];
				break;
			}

		for (int i = 2; i < args.length - 1; i++)
			if (args[i].equals("-translate")) {
				String targetDialect = args[i + 1];
				sql = SqlTranslate.translateSql(sql, targetDialect, sessionId, oracleTempSchema);
			}
		writeFile(sql, args[1]);
	}

	private static void printUsage() {
		System.out.println("SqlRender");
		System.out.println("  Translates OHDSI SQL into one of the supported target SQL dialects, and renders the SQL according to user-specified parameters. See https://github.com/OHDSI/SqlRender for details.");
		System.out.println("");
		System.out.println("Usage");
		System.out.println("  java -jar SqlRender.jar <input file> <output file> [options]");
		System.out.println("");
		System.out.println("Options");
		System.out.println("  -render {<name> <value>} ...    Render the SQL with a list of parameter name-value pairs");
		System.out.println("  -translate <target dialect>     Translate the input SQL to the target dialect");
		System.out.println("  -oracle_temp_schema <schema>    When translating to Oracle SQL, use this schema to emulate temp tables");
		System.out.println("  -session_id <session id>        When translating to Oracle SQL, use this ID to make emulated temp table names unique. Should be 8 chars long");
		System.out.println("");
		System.out.println("Examples");
		System.out.println("  java -jar SqlRender.jar in.sql out.sql -translate postgresql");
		System.out.println("  java -jar SqlRender.jar in.sql out.sql -translate oracle -render cdm_database_schema cdm_synpuf target_database_schema scratch");
	}

	private static String readFile(String fileName) {
		StringBuilder text = new StringBuilder();
		BufferedReader bufferedReader = null;
		try {
			bufferedReader = new BufferedReader(new FileReader(fileName));

			String line = bufferedReader.readLine();

			while (line != null) {
				text.append(line);
				text.append('\n');
				line = bufferedReader.readLine();
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				bufferedReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return text.toString();
	}

	private static void writeFile(String sql, String fileName) {
		BufferedWriter writer = null;
		try {
			writer = new BufferedWriter(new FileWriter(fileName));
			writer.write(sql);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				writer.close();
			} catch (Exception e) {
			}
		}
	}
}
