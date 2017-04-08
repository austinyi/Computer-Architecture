import Multiplexer::*;

interface BarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt, Bit#(1) shiftValue);
    	for(Integer i = 0; i<5; i = i +1)
	begin
		Bit#(32) zero = zeroExtend(shiftValue);
		Bit#(32) sav = val /fromInteger(2**(2**i)) + fromInteger(2**32-2**(32-2**i))*(zero);
		val = multiplexer32(shiftAmt[i],val,sav);
	end
 	return val;
  	endmethod
endmodule

interface BarrelShifterRightLogical;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
  let mbsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
    	Bit#(32) ans <-  mbsr.rightShift(val,shiftAmt,0);
  	return ans;
  endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
	Bit#(32) ans <- bsr.rightShift(val,shiftAmt,val[31]);
	return ans;
  endmethod
endmodule

