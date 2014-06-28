--
--
--  This file is a part of JOP, the Java Optimized Processor
--
--  Copyright (C) 2014, Tórur Biskopstø Strøm (torur.strom@gmail.com)
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--


--
--	ihlu.vhd
--
--	hardware locking unit for cmp jop. replaces cmpsync (global lock)
--
--	2014-05-26	main version created

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use ieee.math_real.all;
use work.jop_types.all;
 
entity ihlu is

generic (cpu_cnt : integer := 4; lck_cnt : integer := 32);
	port (
		clock	: in std_logic;
		reset	: in std_logic;		
		
		sync_in	: in sync_in_array_type(0 to cpu_cnt-1);
		sync_out: out sync_out_array_type(0 to cpu_cnt-1) := (others => NO_SYNC)
	);
end ihlu;

architecture rtl of ihlu is

	signal match  		: std_logic;
	signal empty  		: std_logic_vector(0 to lck_cnt-1);
	signal match_ptr,empty_ptr  		: integer range 0 to lck_cnt-1;
	type ENTRY_ARR is array (integer range <>) of std_logic_vector(31 downto 0); -- Sets the lock width
	signal entry  : ENTRY_ARR(0 to lck_cnt-1);
	
	type REQ_CNT_ARR is array (0 to lck_cnt-1) of integer range 0 to 255; -- Defines how many lock entries are allowed
	signal req_cnt : REQ_CNT_ARR;
	subtype CPU_CNT_TYPE is integer range 0 to cpu_cnt-1;
	signal cpu, ram_data_in, ram_data_out : CPU_CNT_TYPE;
	type LCK_CPU_ARR is array (0 to lck_cnt-1) of CPU_CNT_TYPE;
	signal current, queue_head, queue_tail : LCK_CPU_ARR;
	
	signal data_r  : ENTRY_ARR(0 to cpu_cnt-1);
	signal wr_r, op_r, reg_i, reg_o, status, own : std_logic_vector(0 to cpu_cnt-1);
	
	type state_type is (state_idle,state_ram,state_ram_dly,state_req,state_rd);
	signal state: state_type;

	signal ram_write_address, ram_read_address : integer range 0 to lck_cnt*cpu_cnt-1;
	signal ram_we : std_logic;
	
	type RAM_ARR is array(0 to lck_cnt*cpu_cnt-1) of CPU_CNT_TYPE;
   SIGNAL ram : RAM_ARR;
	
	signal tot_lck_cnt : integer range 0 to lck_cnt;
	
begin
	
	out_sync: 
   for i in 0 to cpu_cnt-1 generate
		sync_out(i).halted <= '1' when (sync_in(i).req = '1' or reg_i(i) /= reg_o(i)) else '0';
      sync_out(i).s_out <= sync_in(0).s_in;  -- Bootup signal used in jvm.asm
		sync_out(i).own <= own(i);
		sync_out(i).status <= status(cpu);
   end generate out_sync;
	
	process (clock)
   begin
      if (rising_edge(clock)) then
         if (ram_we = '1') then
            ram(ram_write_address) <= ram_data_in;
         end if;
         ram_data_out <= ram(ram_read_address);
      end if;
   end process;
	
	empty_encoder: process(clock,reset)
	begin
		if(reset='1') then
			empty_ptr <= 0;
		elsif(rising_edge(clock)) then
			empty_ptr <= 0;
			for i in lck_cnt-1 downto 0 loop
				if (empty(i) = '1') then
					empty_ptr <= i;
				end if;
			end loop;
		end if;
	end process;
	
	match_encoder: process(clock,reset)
	begin
		if(reset='1') then
			match_ptr <= 0;
			match <= '0';
		elsif(rising_edge(clock)) then
			match_ptr <= 0;
			match <= '0';
			for i in lck_cnt-1 downto 0 loop
				if(entry(i) = data_r(cpu) and empty(i) = '0') then
					match_ptr <= i;
					match <= '1';
				end if;
			end loop;
		end if;
	end process;
	
	register_fill: process(clock,reset)
	begin
		if(reset='1') then
			wr_r <= (others => '0');
			op_r <= (others => '0');
			data_r <= (others =>(others => '0'));
			reg_i <= (others => '0');
		elsif(rising_edge(clock)) then
			for i in 0 to cpu_cnt-1 loop
				if(sync_in(i).req = '1' and reg_i(i) = reg_o(i)) then
					op_r(i) <= sync_in(i).op;
					wr_r(i) <= sync_in(i).wr;
					data_r(i) <= sync_in(i).data;
					reg_i(i) <= not(reg_i(i));
				end if;
			end loop;
		end if;
	end process;
	
	
	statemachine: process(clock,reset)
   begin
		if(reset='1') then
			cpu <= 0;
			state <= state_idle;
			
			reg_o <= (others => '0');
			
			ram_read_address <= 0;
			ram_write_address <= 0;
			ram_data_in <= 0;
			
			queue_head <= (others => 0);
			queue_tail <= (others => 0);
			req_cnt <= (others => 0);
			entry <= (others => (others => '0'));
			empty <= (others => '1');
			current <= (others => 0);
			ram_we <= '0';
			tot_lck_cnt <= 0;
			status <= (others => '0');
			own <= (others => '0');
		elsif(rising_edge(clock)) then
			ram_we <= '0';
			
			case state is
				when state_idle =>
					if(reg_i(cpu) /= reg_o(cpu)) then
						if(wr_r(cpu) = '0') then
							state <= state_rd;
						else
							state <= state_ram;
						end if;
					else
						if(cpu = cpu_cnt-1) then
							cpu <= 0;
						else
							cpu <= cpu+1;
						end if;
					end if;
				when state_rd =>
					state <= state_idle;
					reg_o(cpu) <= not(reg_o(cpu));
					if(current(match_ptr) = cpu and empty(match_ptr) = '0') then 
						own(cpu) <= '1';
					else
						own(cpu) <= '0';
					end if;
				when state_ram =>
					state <= state_ram_dly;

	--constant cpu_cnt_width : integer := integer(ceil(log2(real(cpu_cnt))));
	--constant lock_cnt_width : integer := integer(ceil(log2(real(lock_cnt)))); 
	
					ram_read_address <= match_ptr*cpu_cnt + queue_head(match_ptr);
					ram_write_address <= match_ptr*cpu_cnt + queue_tail(match_ptr);
					ram_data_in <= cpu;
					
				when state_ram_dly =>
					state <= state_req;
				when state_req =>
					state <= state_idle;
					reg_o(cpu) <= not(reg_o(cpu));
					status(cpu) <= '0';
					if(match = '1') then
						if(op_r(cpu) = '0') then
							if(current(match_ptr) = cpu) then 
								-- Current cpu is owner so increment entry
								req_cnt(match_ptr) <= req_cnt(match_ptr)+1;
								own(cpu) <= '1';
							else
								-- Current cpu is not owner so enqueue
								ram_we <= '1'; -- Writes cpu to the address written at the previous pipeline stage
								queue_tail(match_ptr) <= queue_tail(match_ptr)+1;
								own(cpu) <= '0';
							end if;
						else
							-- Erase lock
							-- We assume that only the current owner will try to modify the lock
							-- (Enqueued cores should be blocked)
							if(req_cnt(match_ptr) = 0) then 
								-- Current cpu is finished with lock
								if(queue_head(match_ptr) = queue_tail(match_ptr)) then
									-- Queue is empty
									empty(match_ptr) <= '1';
									tot_lck_cnt <= tot_lck_cnt-1;
								else
									-- Unblock next cpu
									current(match_ptr) <= ram_data_out;
									queue_head(match_ptr) <= queue_head(match_ptr)+1;
								end if;
							else
								req_cnt(match_ptr) <= req_cnt(match_ptr)-1;
							end if;
						end if;
					else
						if(op_r(cpu) = '0') then
							if(tot_lck_cnt = lck_cnt) then
								-- No lock entries left so return error
								status(cpu) <= '1';
							else
								empty(empty_ptr) <= '0';
								entry(empty_ptr) <= data_r(cpu);
								current(empty_ptr) <= cpu;
								tot_lck_cnt <= tot_lck_cnt+1;
								own(cpu) <= '1';
							end if;
						else
							-- Trying to release non-existent lock
							status(cpu) <= '1';
						end if;
					end if;
			end case;
		end if;
   end process;
end rtl;