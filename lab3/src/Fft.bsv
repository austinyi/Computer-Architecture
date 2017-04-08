import Vector::*;

import FftCommon::*;
import Fifo::*;

interface Fft;
  method Action enq(Vector#(FftPoints, ComplexData) in);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
endinterface

(* synthesize *)
module mkFftCombinational(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction
  
  rule doFft;
    inFifo.deq;
    Vector#(4, Vector#(FftPoints, ComplexData)) stage_data;
    stage_data[0] = inFifo.first;

    for (StageIdx stage = 0; stage < 3; stage = stage + 1)
      stage_data[stage+1] = stage_f(stage, stage_data[stage]);
    outFifo.enq(stage_data[3]);
  endrule
  
  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftFolded(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(16, Bfly4) bfly <- replicateM(mkBfly4);
  Reg#(StageIdx) stage <- mkReg(0);
  Reg#(Vector#(FftPoints, ComplexData)) sReg <- mkReg(replicate(0));
   // You can copy & paste the stage_f function in the combinational implementation. 

  function Vector#(FftPoints, ComplexData) stage_f(Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction
  rule doFft;
   	Vector#(FftPoints, ComplexData) sxIn;
	Vector#(FftPoints, ComplexData) sxOut;
	if(stage == 0)
		begin sxIn = inFifo.first(); inFifo.deq(); end
	else sxIn = sReg;
	sxOut =  stage_f(sxIn);
	if (stage == 2) outFifo.enq(sxOut);
	else sReg <= sxOut;
	stage <= (stage == 2)? 0 : (stage+1);
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftPipelined(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(3, Vector#(16, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));
  Reg#(Maybe#(Vector#(64,ComplexData))) reg1 <- mkReg(Invalid);
  Reg#(Maybe#(Vector#(64,ComplexData))) reg2 <- mkReg(Invalid);

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end
    stage_out = permute(stage_temp);
    return stage_out;
  endfunction
  
  rule doFft;
   	if(inFifo.notEmpty())
		begin
			inFifo.deq;
			reg1 <= tagged Valid stage_f(0,inFifo.first()); end
		else reg1 <= Invalid;
	case(reg1) matches
		tagged Valid. sx1: begin
		reg2<= tagged Valid stage_f(1,sx1); end
		tagged Invalid: reg2 <= Invalid; endcase
	case(reg2) matches
		tagged Valid. sx2: outFifo.enq(stage_f(2,sx2));
	endcase
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

interface SuperFoldedFft#(numeric type radix);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
  method Action enq(Vector#(FftPoints, ComplexData) in);
endinterface

module mkFftSuperFolded(SuperFoldedFft#(radix)) provisos(Div#(TDiv#(FftPoints, 4), radix, times), Mul#(radix, times, TDiv#(FftPoints, 4)));
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(radix, Bfly4) bfly <- replicateM(mkBfly4);

  Reg#(StageIdx) stage <- mkReg(0);
  Reg#(Vector#(FftPoints, ComplexData)) sReg <- mkReg(replicate(0));
  Reg#(FftIdx) i <- mkReg(0);
  // You can copy & paste the stage_f function in the combinational implementation. 
  // but some modification would be needed to stage_f function in this implementation.
  // or divide stage_f function into doFft rule with appropriate modification.
  function Vector#(FftPoints, ComplexData) stage_f(Vector#(FftPoints, ComplexData) stage_in);
    	Vector#(FftPoints, ComplexData) stage_temp, stage_out;
		stage_temp = stage_in;

	for (FftIdx bf = 0; bf < fromInteger(valueOf(radix)); bf = bf + 1)
		begin
	    FftIdx idx = (i * fromInteger(valueOf(radix)) * 4) + (4 * bf);
        Vector#(4, ComplexData) x;
        Vector#(4, ComplexData) twid;
	    for(FftIdx j = 0; j < 4; j = j + 1 )
            begin
     	  		x[j] = stage_in[idx + j];
 	        	twid[j] = getTwiddle(stage, idx + j);
            end
        let y = bfly[bf].bfly4(twid, x);		
        for(FftIdx j = 0; j < 4; j = j + 1 )
				stage_temp[idx + j] = y[j];
		end
  	    stage_out = stage_temp;
		if( i == fromInteger(valueOf(times)) - 1 )
			stage_out =  permute(stage_temp);
        return stage_out;
  endfunction

  rule doFft;
	Vector#(FftPoints, ComplexData) sxIn;
	Vector#(FftPoints, ComplexData) sxOut;
	if(stage == 0 && i == 0)
		begin sxIn = inFifo.first(); inFifo.deq(); end
	else sxIn = sReg;
	sxOut =  stage_f(sxIn);
	if (stage == 2 && i == fromInteger(valueOf(times)) - 1) outFifo.enq(sxOut);
	else sReg <= sxOut;
	if( i == fromInteger(valueOf(times)) - 1 )
		begin
		i <= 0;
		stage <= (stage == 2)? 0 : stage + 1;
		end
	else i <= i + 1;
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

function Fft getFft(SuperFoldedFft#(radix) f);
  return (interface Fft;
    method enq = f.enq;
    method deq = f.deq;
  endinterface);
endfunction

(* synthesize *)
module mkFftSuperFolded4(Fft);
  SuperFoldedFft#(16) sfFft <- mkFftSuperFolded;
  // TODO: Change the number at SuperFoldedFft#(x) by 1, 2, 4, 8 to test polymorphism of your super folded implementation.
  return (getFft(sfFft));
endmodule
