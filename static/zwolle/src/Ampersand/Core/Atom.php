<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use DateTime;
use DateTimeZone;
use JsonSerializable;
use Ampersand\Log\Logger;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Atom implements JsonSerializable {
	/**
	 *
	 * @var \Psr\Log\LoggerInterface
	 */
	protected $logger;
	
	/**
	 * Ampersand identifier of the atom
	 * @var string
	 */
	public $id;
	
	/**
	 * Specifies the concept of which this atom is an instance
	 * @var Concept
	 */
	public $concept;
    
    /**
     * @var array|null $queryData the row data (from database query) from which this resource is created
     */
    protected $queryData = null;
    
	/**
	 * Atom constructor
	 * @param string $atomId
	 * @param Concept $concept
	 * @return void
	 */
	public function __construct($atomId, Concept $concept){
		$this->logger = Logger::getLogger('ATOM');
		
        $this->concept = $concept;
        
        switch($this->concept->type){
            case "ALPHANUMERIC" :
            case "BIGALPHANUMERIC" :
            case "HUGEALPHANUMERIC" :
            case "PASSWORD" :
            case "TYPEOFONE" :
            case "BOOLEAN" :
                $this->id = $atomId;
                break;
            case "DATE" :
                // In php backend, all Dates are kept in ISO-8601 format
                $datetime = new DateTime($atomId);
                $this->id = $datetime->format('Y-m-d'); // format in ISO-8601 standard
                break;
            case "DATETIME" :
                // In php backend, all DateTimes are kept in DateTimeZone::UTC and DateTime::ATOM format
                // $atomId may contain a timezone, otherwise UTC is asumed.
                $datetime = new DateTime($atomId, new DateTimeZone('UTC')); // The $timezone parameter is ignored when the $time parameter either is a UNIX timestamp (e.g. @946684800) or specifies a timezone (e.g. 2010-01-28T15:00:00+02:00).
                $datetime->setTimezone(new DateTimeZone('UTC')); // if not yet UTC, convert to UTC
                $this->id = $datetime->format(DateTime::ATOM); // format in ISO-8601 standard, i.e. 2005-08-15T15:52:01+00:00 (DateTime::ATOM)
                break;
            case "FLOAT" :
            case "INTEGER" :
            case "OBJECT" :
                $this->id = $atomId;
                break;
            default :
                throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept}]'", 501);
        }
	}
	
    /**
     * Function is called when object is treated as a string
     * @return string
     */
	public function __toString(){
        // if atom id is longer than 40 chars, display first and last 20 chars
        $id = strlen($this->id) > 40 ? substr($this->id, 0, 20) . '...' . substr($this->id, -20) : $this->id;
	    return "{$id}[{$this->concept}]";
	}
    
    /**
     * Return label of atom to be displayed in user interfaces
     * for Atoms this is the same as the Atom identifier
     * @return string
     */
    public function getLabel(){
        return $this->id;
    }
    
    /**
     * Returns json representation of Atom (identifier) according to Ampersand technical types (TTypes)
     * Function is called when object encoded to json with json_encode()
     * @throws Exception when technical type is not (yet) supported
     * @return mixed
     */
    public function jsonSerialize(){
        switch($this->concept->type){
            case "ALPHANUMERIC" :
            case "BIGALPHANUMERIC" :
            case "HUGEALPHANUMERIC" :
            case "PASSWORD" :
            case "TYPEOFONE" :
                return (string) $this->id;
            case "BOOLEAN" :
                return (bool) $this->id;
            case "DATE" :
                $datetime = new DateTime($this->id);
                return $datetime->format('Y-m-d'); // format in ISO-8601 standard
            case "DATETIME" :
                // DateTime(s) may contain a timezone, otherwise UTC is asumed.
                $datetime = new DateTime($this->id, new DateTimeZone('UTC')); // The $timezone parameter is ignored when the $time parameter either is a UNIX timestamp (e.g. @946684800) or specifies a timezone (e.g. 2010-01-28T15:00:00+02:00).
                $datetime->setTimezone(new DateTimeZone(date_default_timezone_get())); // convert back to systemtime
                return $datetime->format(DateTime::ATOM); // format in ISO-8601 standard, i.e. 2005-08-15T15:52:01+00:00 (DateTime::ATOM)
            case "FLOAT" :
                return (float) $this->id;
            case "INTEGER" :
                return (int) $this->id;
            case "OBJECT" :
                return rawurlencode($this->id);
            default :
                throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept}]'", 501);
        }
    }
    
	/**
	 * Checks if atom exists in storage
	 * @return boolean
	 */
	public function exists(){
        return $this->concept->atomExists($this);
	}
	
	/**
	 * Add atom to concept
	 * @return Atom $this
	 */
	public function add(){
        $this->concept->addAtom($this);
	    return $this;
	}
    
    /**
     * Delete atom from concept
     * @return Atom $this
     */
    public function delete(){
        $this->concept->deleteAtom($this);
        return $this;
    }
    
    /**
     * @param string|Atom $tgtAtom
     * @param string|Relation $relation when provided as string, use relation signature
     * @param boolean $isFlipped specifies if $this and $tgtAtom must be flipped to match the relation
     * @return Link
     */
    public function link($tgtAtom, $relation, $isFlipped = false){
        if(!($relation instanceof Relation)) $relation = Relation::getRelation($relation);
        if(!($tgtAtom instanceof Atom)) $tgtAtom = $isFlipped ? new Atom($tgtAtom, $relation->srcConcept) : new Atom($tgtAtom, $relation->tgtConcept);        
        
        if($isFlipped) return new Link($relation, $tgtAtom, $this);
        else return new Link($relation, $this, $tgtAtom);
    }
    
    /**
     * @param string|Relation $relation when provided as string, use relation signature
     * @param boolean $isFlipped specifies if relation must be flipped
     * @return Link[]
     */
    public function getLinks($relation, $isFlipped = false){
        if(!($relation instanceof Relation)) $relation = Relation::getRelation($relation);
        
        if($isFlipped) return $relation->getAllLinks(null, $this);
        else return $relation->getAllLinks($this, null);
    }
    
    /**
     * Save query row data (can be used for subinterfaces)
     * @param arry $queryData 
     * @return void
     */
    public function setQueryData($data){
        $this->queryData = $data;
    }
    
    /**
     * Get (column of) query data
     * @param string $colName
     * @param boolean $exists reference var that returns if column exists
     * @return string|array
     */
    public function getQueryData($colName = null, &$exists = null){
        if(is_null($colName)){
            return (array) $this->queryData;
        }else{
            // column name is prefixed with 'ifc_' to prevent duplicates with 'src' and 'tgt' cols, which are standard added to query data
            $exists = array_key_exists($colName, (array) $this->queryData);
            return $this->queryData[$colName];
        }
    }
}

?>