/*
  This file is part of JOP, the Java Optimized Processor
    see <http://www.jopdesign.com/>

  Copyright (C) 2010, Martin Schoeberl (martin@jopdesign.com)

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
package jembench;

/**
 * Interface for parallel applications with independent
 * units of work.
 * @author martin
 *
 */
public interface WorkUnit {

	/**
	 * Execute unit i of work.
	 * @param i
	 */
	void executeUnit(int i);

	/**
	 * Get the number of units of work to be executed.
	 * @return
	 */
	int getNrOfUnits();

}
