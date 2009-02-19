/*
  This file is part of JOP, the Java Optimized Processor
    see <http://www.jopdesign.com/>

  Copyright (C) 2008-2009, Benedikt Huber (benedikt.huber@gmail.com)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.jopdesign.wcet08;

import java.io.File;

import com.jopdesign.wcet08.config.BooleanOption;
import com.jopdesign.wcet08.config.Config;
import com.jopdesign.wcet08.config.IntegerOption;
import com.jopdesign.wcet08.config.Option;
import com.jopdesign.wcet08.config.StringOption;

public class ProjectConfig {
	public static final StringOption PROJECT_NAME =
		new StringOption("projectname","name of the 'project', used when generating reports",true);
	public static final StringOption APP_CLASS_NAME = 
		new StringOption("app-class",
			             "the name of the class containing the main entry point of the RTJava application",
			             false);
	public static final StringOption TARGET_METHOD =
		new StringOption("target-method",
						 "the name (optional: class,signature) of the method to be analyzed",
						 "measure");

	public static final StringOption TARGET_CLASSPATH =  new StringOption("cp","the classpath",false);
	public static final StringOption TARGET_SOURCEPATH = new StringOption("sp","the sourcepath",false);
	
	public static final StringOption OUT_DIR =
		 new StringOption("outdir","directory for output of the analysis tool","java/target/wcet/");

	private static final BooleanOption DO_GENERATE_REPORTS =
		new BooleanOption("report-generation","whether reports should be generated",true);
	public static final BooleanOption DO_DFA =
		new BooleanOption("dataflow-analysis","whether dataflow analysis should be performed",false);
	public static final BooleanOption USE_UPPAAL =
		new BooleanOption("uppaal","perform uppaal-based WCET analysis",false);
	public static final IntegerOption UPPAAL_COMPLEXITY_TRESHOLD =
		new IntegerOption("uppaal-treshold","limit UPPAAL to methods below the given expanded cyclomatic complexity",true);
	public static final Option<?>[] projectOptions =
	{ 
		OUT_DIR,
		APP_CLASS_NAME, TARGET_METHOD, PROJECT_NAME,
		TARGET_CLASSPATH, TARGET_SOURCEPATH,
		DO_DFA, USE_UPPAAL, UPPAAL_COMPLEXITY_TRESHOLD
	};
	public static File getOutDir(String subdir) {
		File dir = new File(
				new ProjectConfig(Config.instance()).getOutDir(),
				subdir);
		dir.mkdir();
		return dir;
	}
	/** If no instance of project is available, us this one to get a path for
	 * writing a file (not recommended, but sometimes useful for debugging stuff)
	 * @param subdir
	 * @param name 
	 */
	public static File getOutFile(String subdir, String name) {
		return new File(getOutDir(subdir),Config.sanitizeFileName(name));
	}
	public String getProjectName() {
		return config.getOptionWithDefault(PROJECT_NAME,
				Config.sanitizeFileName(getAppClassName() + "_" + getTargetMethodName()));
	}
	public File getOutDir() {
		return new File(config.getOption(OUT_DIR),getProjectName());
	}
	private Config config;
	public ProjectConfig(Config config) {
		this.config = config;
	}
	/** Return the configured classpath, a list of path separated by {@see File.pathSeparatorChar}
	 * @return the classpath used for looking up compiled class files
	 */
	public String getClassPath() {
		return config.getOption(TARGET_CLASSPATH);
	}
	/**
	 * Get the name of the application class defining the entry point main()
	 * @return
	 */
	public String getAppClassName() {
		String appClass = config.getOption(APP_CLASS_NAME);
		if(appClass.indexOf('/') > 0) {
			appClass = appClass.replace('/','.');			
		}
		return appClass;
	}
	/**
	 * get the name of the method to be analyzed
	 * @return
	 */
	public String getTargetMethodName() {
		return config.getOption(ProjectConfig.TARGET_METHOD);
	}
	public String getTargetClass() {
		String measureClass = splitFQMethod(getTargetMethodName(),true);
		if(measureClass == null) return getAppClassName();
		else return measureClass;
	}
	public String getTargetMethod() {
		return splitFQMethod(getTargetMethodName(),false);
	}

	/**
	 * A list of paths (seperated by pathSeparatorChar) used for looking up sources
	 * @return the path to source directories
	 */
	public String getSourcePath() {
		return config.getOption(TARGET_SOURCEPATH);		
	}
	/**
	 * Whether reports should be generated
	 * @return
	 */
	public boolean doGenerateReport() {
		return config.getOption(DO_GENERATE_REPORTS);
	}
	public File getReportDir() {
		return new File(getOutDir(),"report");
	}
	public boolean doDataflowAnalysis() {
		return config.getOption(DO_DFA);
	}
	public boolean useUppaal() {
		return config.getOption(USE_UPPAAL);
	}
	public boolean hasUppaalComplexityTreshold() {
		return config.hasOption(UPPAAL_COMPLEXITY_TRESHOLD);
	}
	public Long getUppaalComplexityTreshold() {
		return config.getOption(UPPAAL_COMPLEXITY_TRESHOLD);
	}
	public static String splitFQMethod(String s, boolean getClass) {		
		int sigIx = s.indexOf('(');
		String sWithoutSig;
		if(sigIx > 0) {
			sWithoutSig = s.substring(0,sigIx);
		} else {
			sWithoutSig = s;
		}
		int methIx = sWithoutSig.lastIndexOf('.');
		if(getClass) {
			if(methIx > 0) {
				return s.substring(0,methIx);
			} else {
				return null;
			}
		} else {
			if(methIx > 0) {
				return s.substring(methIx + 1);
			} else {
				return s;
			}
		}
	}
	public Config getConfigManager() {
		return this.config;
	}
}