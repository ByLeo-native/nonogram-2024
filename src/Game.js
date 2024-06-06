import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import ToggleSwitch from './ToggleSwitch';
import RevealButton from './RevealButton';
import WinnerBox from './WinnerBox';
import SolutionButton from './SolutionButton';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsCluesState, setRowsCluesState] = useState(null);
  const [colsCluesState, setColsCluesState] = useState(null);
  const [isPaintedMode, setIsPaintedMode] = useState(true);
  const [winner, setWinner] = useState(null);


  const [revealMode, setRevealMode] = useState(false);
  const [showSolution, setShowSolution] = useState(false);
  const [solutionGrid, setSolutionGrid] = useState(null);

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
        
        /**
         * Se realiza una nueva peticion para verificar si el nonograma cuenta con alguna restricción cumplida.
         * Se utiliza la respuesta de la primera peticion en vez de las constantes declaradas porque falla.
         */
        const g = JSON.stringify(response['Grid']).replaceAll('"_"', '_');
        const rClues = JSON.stringify(response['RowClues']);
        const cClues = JSON.stringify(response['ColumClues']);
        const querySS = `check_clues(${g}, ${rClues}, ${cClues}, StatusOfRows, StatusOfCols)`;
        setWaiting(true);
        pengine.query(querySS, (success, response) => {
          if(success) {
            setRowsCluesState(response['StatusOfRows'].map(element => {return element === 1 ? true : false;}));
            setColsCluesState(response['StatusOfCols'].map(element => {return element === 1 ? true : false;}));
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
        /**
         * Si se verifica las restricciones en fila y en columna en simultaneo, entonces se verifica si se resolvio el nonograma.
         * VERIFICACION DE NONOGRAMA
         */
        if (response['RowSat'] === 1 && response['ColSat'] === 1) {
          const g = JSON.stringify(response['ResGrid']).replaceAll('"_"', '_');
          const querySS = `solve(${g}, ${rowsCluesS}, ${colsCluesS}, Solved)`;
          pengine.query(querySS, (success, response) => {
            if(success) {
              if(response['Solved']) {
                statusText = `¡Has completado el nonograma!`;
                setWinner(statusText);
              } else {
                console.log(`No esta resuelto`);
              }
            } else {
              console.error(`La solicitud de solve no fue exitosa`);
            }
          });
        }
        setWaiting(false);
      }
    })
  }

  function revealCell(i, j) {
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);

    const queryS = `reveal([${i},${j}], ${rowsCluesS}, ${colsCluesS}, RevealedContent)`;
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        const newGrid = [...grid];
        newGrid[i][j] = response['RevealedContent'];
        setGrid(newGrid);
      }
      setWaiting(false);
    });
  }

  if (!grid) {
    return null;
  }
  
  let statusText = 'Keep playing!';
  
  return (
    <div className="game">
      <div className="container">
        <RevealButton  // <-- Usa el nuevo componente RevealButton
          revealMode={revealMode}
          setRevealMode={setRevealMode}
        />
        <SolutionButton
          solutionMode={showSolution}
          setSolutionMode={setShowSolution}
        />
      </div>
      
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