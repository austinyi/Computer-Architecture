import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
	method Action shift_request(Bit#(32) operand, Bit#(5) shamt, Bit#(1) val);
	method ActionValue#(Bit#(32)) shift_response();
endinterface

/* Interface of the three shifter modules
 *
 * They have the same interface.
 * So, we just copy it using typedef declarations.
 */
interface BarrelShifterRightLogicalPipelined;
	method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
	method ActionValue#(Bit#(32)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
	/* Implement left shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
		bsrp.shift_request(reverseBits(operand),shamt,0);
	endmethod

	method ActionValue#(Bit#(32)) shift_response();	
		Bit#(32) rev <- bsrp.shift_response();
		return reverseBits(rev);
	endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
	/* Implement right logical shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
		bsrp.shift_request(operand,shamt,0);
	endmethod

	method ActionValue#(Bit#(32)) shift_response();
		Bit#(32) ans <- bsrp.shift_response();
		return ans;
	endmethod
endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
	/* Implement right arithmetic shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
		bsrp.shift_request(operand,shamt,operand[31]);
	endmethod

	method ActionValue#(Bit#(32)) shift_response();
		Bit#(32) ans <- bsrp.shift_response();
		return ans;
	endmethod
endmodule

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);

	let inFifo <- mkFIFOF;
	let outFifo <- mkFIFOF;
	
	Reg#(Maybe#(Bit#(32))) reg16 <- mkReg(Invalid);
        Reg#(Maybe#(Bit#(32))) reg8 <- mkReg(Invalid);
        Reg#(Maybe#(Bit#(32))) reg4 <- mkReg(Invalid);
        Reg#(Maybe#(Bit#(32))) reg2 <- mkReg(Invalid);
        
	Reg#(Bit#(1)) shamt16 <- mkReg(0);
	Reg#(Bit#(2)) shamt8 <- mkReg(0);
	Reg#(Bit#(3)) shamt4 <- mkReg(0);
	Reg#(Bit#(4)) shamt2 <- mkReg(0);
	
	Reg#(Bit#(1)) val16 <- mkReg(0);
        Reg#(Bit#(1)) val8 <- mkReg(0);
        Reg#(Bit#(1)) val4 <- mkReg(0);
        Reg#(Bit#(1)) val2 <- mkReg(0);

	function Bit#(32) shft16(Bit#(32) operand, Bit#(1) shamt, Bit#(1) val);
		Bit#(32) zero = zeroExtend(val);
                Bit#(32) sav = operand /fromInteger(2**(2**4)) + fromInteger(2**32-2**(32-2**4))*(zero);
                operand = multiplexer32(shamt[0],operand,sav);
		return operand;
	endfunction

	function Bit#(32) shft8(Bit#(32) operand, Bit#(2) shamt, Bit#(1) val);
                Bit#(32) zero = zeroExtend(val);
                Bit#(32) sav = operand /fromInteger(2**(2**3)) + fromInteger(2**32-2**(32-2**3))*(zero);
                operand = multiplexer32(shamt[0],operand,sav);
                return operand;
        endfunction

	function Bit#(32) shft4(Bit#(32) operand, Bit#(3) shamt, Bit#(1) val);
                Bit#(32) zero = zeroExtend(val);
                Bit#(32) sav = operand /fromInteger(2**(2**2)) + fromInteger(2**32-2**(32-2**2))*(zero);
                operand = multiplexer32(shamt[0],operand,sav);
                return operand;
        endfunction

	function Bit#(32) shft2(Bit#(32) operand, Bit#(4) shamt, Bit#(1) val);
                Bit#(32) zero = zeroExtend(val);
                Bit#(32) sav = operand /fromInteger(2**(2**1)) + fromInteger(2**32-2**(32-2**1))*(zero);
                operand = multiplexer32(shamt[0],operand,sav);
                return operand;
        endfunction

	function Bit#(32) shft1(Bit#(32) operand, Bit#(5) shamt, Bit#(1) val);
                Bit#(32) zero = zeroExtend(val);
                Bit#(32) sav = operand /fromInteger(2**(2**0)) + fromInteger(2**32-2**(32-2**0))*(zero);
                operand = multiplexer32(shamt[0],operand,sav);
                return operand;
        endfunction

	rule shift;
		if(inFifo.notEmpty())
			begin
				reg2 <= tagged Valid shft1(tpl_1(inFifo.first()),tpl_2(inFifo.first()),tpl_3(inFifo.first()));
				shamt2 <= truncateLSB(tpl_2(inFifo.first()));
				val2 <= tpl_3(inFifo.first());
				inFifo.deq(); end
			else reg2 <= Invalid;
		case (reg2) matches
                        tagged Valid .sx2: begin
						reg4 <= tagged Valid shft2(sx2,shamt2,val2);
						shamt4 <= truncateLSB(shamt2); 
						val4 <= val2; end
                        tagged Invalid: reg4 <= Invalid; endcase
		case (reg4) matches
			tagged Valid .sx4: begin
						reg8 <= tagged Valid shft4(sx4,shamt4,val4);
						shamt8 <= truncateLSB(shamt4);
						val8 <= val4; end
			tagged Invalid: reg8 <= Invalid; endcase
		case (reg8) matches
			tagged Valid. sx8: begin
						reg16 <= tagged Valid shft8(sx8,shamt8,val8);
						shamt16 <= truncateLSB(shamt8);
						val16 <= val8; end
			tagged Invalid: reg16 <= Invalid; endcase
		case (reg16) matches
			tagged Valid. sx16: outFifo.enq(shft16(sx16,shamt16,val16));
		endcase
	endrule

	method Action shift_request(Bit#(32) operand, Bit#(5) shamt, Bit#(1) val);
		inFifo.enq(tuple3(operand,shamt,val));
	endmethod

	method ActionValue#(Bit#(32)) shift_response();
		outFifo.deq;
		return outFifo.first;
	endmethod
endmodule
