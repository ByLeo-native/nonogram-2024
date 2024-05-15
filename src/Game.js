import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import ToggleSwitch from './ToggleSwitch';
import WinnerBox from './WinnerBox';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsCluesState, setRowsCluesState] = useState(null);
  const [colsCluesState, setColsCluesState] = useState(null);
  const [checkIfTheNonogramIsResolved, setCheckIfTheNonogramIsResolved] = useState(false);
  const [isPaintedMode, setIsPaintedMode] = useState(true);
  const [winner, setWinner] = useState(null)
  

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);  
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    let queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
        setRowsCluesState(Array(response['RowClues'].length).fill(false));
        setColsCluesState(Array(response['ColumClues'].length).fill(false));
        
        
        const g = JSON.stringify(response['Grid']).replaceAll('"_"', '_');
        const rClues = JSON.stringify(response['RowClues']);
        const cClues = JSON.stringify(response['ColumClues']);
        console.log(g);
        const querySS = `check_clues(${g}, ${rClues}, ${cClues}, StatusOfRows, StatusOfCols)`;
        setWaiting(true);
        pengine.query(querySS, (success, response) => {
          if(success) {
            console.log(`check_clues exitoso ${response}`);
            setRowsCluesState(response['StatusOfRows'].map(element => {return element === 1 ? true : false;}));
            setColsCluesState(response['StatusOfCols'].map(element => {return element === 1 ? true : false;}));
            console.log(response['StatusOfRows']);
            console.log(response['StatusOfCols']);
          } else {
            console.error(`Error cuando se solicita check_clues`);
            console.log(response);
          }
          setWaiting(false);
        })
      }
    });
  }

  function updateClues(row, col, RowSat, ColSat) {
    const updatedRowsCluesState = [...rowsCluesState];
    // Aquí actualiza las Clues en fila en función de RowSat
    updatedRowsCluesState[row] = RowSat === 1;
    // Actualiza el estado de las Clues en columna
    const updatedColsCluesState = [...colsCluesState];
    // Aquí actualiza las Clues en columna en función de ColSat
    updatedColsCluesState[col] = ColSat === 1;
    setRowsCluesState(updatedRowsCluesState);
    setColsCluesState(updatedColsCluesState);
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);

    const content = isPaintedMode ? '#' : 'X';
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
      updateClues(i, j, response['RowSat'], response['ColSat']);
      if (response['RowSat'] === 1 || response['ColSat'] === 1) {
        setCheckIfTheNonogramIsResolved(true);
      }
      // Llama a isNonogramSolved dentro del callback de pengine.query
      if (checkIfTheNonogramIsResolved) {
        isNonogramSolved();
      }
      } else {
        console.error("La consulta no tuvo éxito.");
      }
      setWaiting(false);
    });
  }

  function isNonogramSolved() {
    setCheckIfTheNonogramIsResolved(false);
    console.log(grid);
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `solve(${squaresS}, ${rowsCluesS}, ${colsCluesS}, Solved)`;
    pengine.query( queryS, (success, response) => {
      if(success) {
        if(response['Solved']) {
          statusText = `¡Has completado el nonograma!`;
          setWinner(statusText);
        } else {
          console.log(`No esta resuelto`);
        }
      } else {
        console.error(`No hubo exito con el solve`);
      }
    })
  }

  if (!grid) {
    return null;
  }
  
  let statusText = 'Keep playing!';
  
  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        onClick={(i, j) => handleClick(i, j)}
        rowsCluesState={rowsCluesState}
        colsCluesState={colsCluesState}
      />
      <ToggleSwitch 
        paintedMode={isPaintedMode}
        setPaintedMode={setIsPaintedMode}
      />
      <div className="game-info">
        {statusText}
      </div>
      <WinnerBox 
        winner={winner}
      />
    </div>
  );
}

export default Game;