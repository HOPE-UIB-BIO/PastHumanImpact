-- Run citeproc once so citation numbers remain continuous, then place the
-- submitted main and Methods reference groups at their source positions.

local function is_div(block, identifier)
  return block.t == "Div" and block.identifier == identifier
end

local protected_crossrefs = {}

local function is_crossref(cite)
  if #cite.citations == 0 then
    return false
  end
  for _, citation in ipairs(cite.citations) do
    local identifier = citation.id
    local supported = identifier:match("^fig%-")
      or identifier:match("^tbl%-")
      or identifier:match("^suppfig%-")
      or identifier:match("^supptbl%-")
    if not supported then
      return false
    end
  end
  return true
end

local function protect_crossrefs(document)
  return document:walk({
    Cite = function(cite)
      if is_crossref(cite) then
        protected_crossrefs:insert(cite)
        local index = tostring(#protected_crossrefs)
        return pandoc.Span(
          {pandoc.Str("crossref-placeholder-" .. index)},
          pandoc.Attr("", {}, {{"data-crossref-index", index}})
        )
      end
    end
  })
end

local function restore_crossrefs(document)
  return document:walk({
    Span = function(span)
      local index = tonumber(span.attributes["data-crossref-index"])
      if index ~= nil then
        return protected_crossrefs[index]
      end
    end
  })
end

function Pandoc(document)
  protected_crossrefs = pandoc.List()
  local protected_document = protect_crossrefs(document)
  local cited = pandoc.utils.citeproc(protected_document)
  cited = restore_crossrefs(cited)
  local bibliography = nil

  for _, block in ipairs(cited.blocks) do
    if is_div(block, "refs") then
      bibliography = block
      break
    end
  end

  if bibliography == nil then
    return cited
  end

  local main_entries = pandoc.List()
  local methods_entries = pandoc.List()

  for _, entry in ipairs(bibliography.content) do
    if entry.t == "Div" and entry.identifier:match("^ref%-methods%-ref%-") then
      methods_entries:insert(entry)
    else
      main_entries:insert(entry)
    end
  end

  local output = pandoc.List()
  for _, block in ipairs(cited.blocks) do
    if is_div(block, "refs") then
      -- The generated bibliography is inserted into the two markers below.
    elseif is_div(block, "main-bibliography") then
      output:insert(pandoc.Div(main_entries, pandoc.Attr("refs", {"references"})))
    elseif is_div(block, "methods-bibliography") then
      output:insert(pandoc.Div(methods_entries, pandoc.Attr("refs", {"references"})))
    else
      output:insert(block)
    end
  end

  cited.blocks = output
  return cited
end

